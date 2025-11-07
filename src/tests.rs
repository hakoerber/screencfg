
use super::*;

enum PlanExpect<'cmd, 'ws, 'out> {
    Error,
    Valid(Plan<'ws, 'out>, &'cmd str),
}

#[test]
fn single_laptop() -> Result<(), Error> {
    let mut connection = i3::MockConnection {
        fail: false,
        setting: i3::MockSetting::LaptopOnly,
    };

    let mut outputs = connection
        .outputs()?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?;
    outputs.sort();

    let workspaces = Workspaces::convert(
        connection.workspaces()?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation: Workstation = outputs[..].try_into()?;

    assert_eq!(
        &workstation,
        &Workstation {
            laptop: Some(&outputs[0]),
            externals: None,
            disconnected_externals: vec![],
        }
    );

    for (setup, expect) in [
        (Setup::LaptopLeft, PlanExpect::Error),
        (Setup::LaptopRight, PlanExpect::Error),
        (
            Setup::LaptopOnly,
            PlanExpect::Valid(
                Plan {
                    output_settings: vec![outputs[0].on()],
                    workspace_settings: vec![],
                },
                "--output eDP-1 --auto",
            ),
        ),
        (Setup::ExternalOnly, PlanExpect::Error),
    ] {
        let result = workstation.plan(setup, &workspaces);
        match expect {
            PlanExpect::Error => assert!(result.is_err()),
            PlanExpect::Valid(plan, cmd) => {
                assert_eq!(result?, plan);
                assert_eq!(
                    plan.commands()
                        .into_iter()
                        .filter_map(|cmd| {
                            match cmd {
                                Command::Xrandr(_cmd, args) => Some(args.join(" ")),
                                Command::MoveWorkspace { .. } => None,
                            }
                        })
                        .next()
                        .unwrap(),
                    cmd
                );
            }
        }
    }

    Ok(())
}

#[test]
fn multiple_laptops() -> Result<(), Error> {
    let laptop1 = Output {
        name: "eDP-1".to_string(),
        class: OutputClass::Laptop,
        connection_state: OutputConnectionState::Connected,
    };

    let laptop2 = Output {
        name: "eDP-2".to_string(),
        class: OutputClass::Laptop,
        connection_state: OutputConnectionState::Connected,
    };

    let outputs = [laptop1, laptop2];

    let workstation: Result<Workstation, Error> = outputs[..].try_into();

    assert!(workstation.is_err());

    Ok(())
}

#[test]
fn no_screens() -> Result<(), Error> {
    let outputs = [];

    let workstation: Result<Workstation, Error> = outputs[..].try_into();

    assert!(workstation.is_err());

    Ok(())
}

#[test]
fn single_external() -> Result<(), Error> {
    let mut connection = i3::MockConnection {
        fail: false,
        setting: i3::MockSetting::ExternalOnly(1),
    };

    let mut outputs = connection
        .outputs()?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?;
    outputs.sort();

    let workspaces = Workspaces::convert(
        connection.workspaces()?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation: Workstation = outputs[..].try_into()?;

    assert_eq!(
        workstation,
        Workstation {
            laptop: None,
            externals: Some((&outputs[0], vec![])),
            disconnected_externals: vec![],
        }
    );

    for (setup, expect) in [
        (Setup::LaptopLeft, PlanExpect::Error),
        (Setup::LaptopRight, PlanExpect::Error),
        (Setup::LaptopOnly, PlanExpect::Error),
        (
            Setup::ExternalOnly,
            PlanExpect::Valid(
                Plan {
                    output_settings: vec![outputs[0].on()],
                    workspace_settings: vec![],
                },
                "--output DP-1 --auto",
            ),
        ),
    ] {
        let result = workstation.plan(setup, &workspaces);
        match expect {
            PlanExpect::Error => assert!(result.is_err()),
            PlanExpect::Valid(plan, cmd) => {
                assert_eq!(result?.output_settings, plan.output_settings);
                assert_eq!(
                    plan.commands()
                        .into_iter()
                        .filter_map(|cmd| {
                            match cmd {
                                Command::Xrandr(_cmd, args) => Some(args.join(" ")),
                                Command::MoveWorkspace { .. } => None,
                            }
                        })
                        .next()
                        .unwrap(),
                    cmd
                );
            }
        }
    }

    Ok(())
}

#[test]
fn multiple_external() -> Result<(), Error> {
    let mut connection = i3::MockConnection {
        fail: false,
        setting: i3::MockSetting::ExternalOnly(2),
    };

    let mut outputs = connection
        .outputs()?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?;
    outputs.sort();

    let workspaces = Workspaces::convert(
        connection.workspaces()?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation: Workstation = outputs[..].try_into()?;

    assert_eq!(
        workstation,
        Workstation {
            laptop: None,
            externals: Some((&outputs[0], vec![&outputs[1]])),
            disconnected_externals: vec![],
        }
    );

    for (setup, expect) in [
        (Setup::LaptopLeft, PlanExpect::Error),
        (Setup::LaptopRight, PlanExpect::Error),
        (Setup::LaptopOnly, PlanExpect::Error),
        (
            Setup::ExternalOnly,
            PlanExpect::Valid(
                Plan {
                    output_settings: vec![outputs[0].on(), outputs[1].on()],
                    workspace_settings: vec![],
                },
                "--output DP-1 --auto --output DP-2 --auto --right-of DP-1",
            ),
        ),
    ] {
        let result = workstation.plan(setup, &workspaces);
        match expect {
            PlanExpect::Error => assert!(result.is_err()),
            PlanExpect::Valid(plan, cmd) => {
                assert_eq!(result?.output_settings, plan.output_settings);
                assert_eq!(
                    plan.commands()
                        .into_iter()
                        .filter_map(|cmd| {
                            match cmd {
                                Command::Xrandr(_cmd, args) => Some(args.join(" ")),
                                Command::MoveWorkspace { .. } => None,
                            }
                        })
                        .next()
                        .unwrap(),
                    cmd
                );
            }
        }
    }

    Ok(())
}

#[test]
fn mixture() -> Result<(), Error> {
    let mut connection = i3::MockConnection {
        fail: false,
        setting: i3::MockSetting::Mixed,
    };

    let mut outputs = connection
        .outputs()?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?;
    outputs.sort();

    let workspaces = Workspaces::convert(
        connection.workspaces()?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation: Workstation = outputs[..].try_into()?;

    assert_eq!(
        workstation,
        Workstation {
            laptop: Some(&outputs[0]),
            externals: Some((&outputs[1], vec![&outputs[2]])),
            disconnected_externals: vec![],
        }
    );

    for (setup, expect) in [
            (
                Setup::LaptopLeft,
                PlanExpect::Valid(
                    Plan {
                        output_settings: vec![
                            outputs[0].on(),
                            outputs[1].on(),
                            outputs[2].on(),
                        ],
                        workspace_settings:vec![],
                    },
                    "--output eDP-1 --auto --output DP-1 --auto --right-of eDP-1 --output HDMI-1 --auto --right-of DP-1",
                ),
            ),
            (
                Setup::LaptopRight,
                PlanExpect::Valid(
                    Plan {
                        output_settings: vec![
                            outputs[1].on(),
                            outputs[2].on(),
                            outputs[0].on(),
                        ],
                        workspace_settings:vec![],
                    },
                    "--output DP-1 --auto --output HDMI-1 --auto --right-of DP-1 --output eDP-1 --auto --right-of HDMI-1",
                ),
            ),
            (
                Setup::LaptopOnly,
                PlanExpect::Valid(
                    Plan {
                        output_settings: vec![
                            outputs[0].on(),
                            outputs[1].off(),
                            outputs[2].off(),
                        ],
                        workspace_settings:vec![],
                    },
                    "--output eDP-1 --auto --output DP-1 --off --output HDMI-1 --off",
                ),
            ),
            (
                Setup::ExternalOnly,
                PlanExpect::Valid(
                    Plan {
                        output_settings: vec![
                            outputs[1].on(),
                            outputs[2].on(),
                            outputs[0].off(),
                        ],
                        workspace_settings:vec![],
                    },
                    "--output DP-1 --auto --output HDMI-1 --auto --right-of DP-1 --output eDP-1 --off",
                ),
            ),
        ] {
            let result = workstation.plan(setup, &workspaces);
            match expect {
                PlanExpect::Error => assert!(result.is_err()),
                PlanExpect::Valid(plan, cmd) => {
                    assert_eq!(result?.output_settings, plan.output_settings);
                    assert_eq!(
                        plan.commands()
                            .into_iter()
                            .filter_map(|cmd| {
                                match cmd {
                                    Command::Xrandr(_cmd, args) => {
                                        Some(args.join(" "))
                                    }
                                    Command::MoveWorkspace { .. } => None,
                                }
                            })
                            .next()
                            .unwrap(),
                        cmd
                    );
                }
            }
        }

    Ok(())
}
