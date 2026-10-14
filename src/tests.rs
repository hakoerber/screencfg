#![expect(clippy::panic, reason = "it's test code")]

use std::sync::Once;

use pretty_assertions::assert_eq;
use tap::Tap as _;
use tracing::Level;

use nonempty::NonEmptyVec;
use sway::Conn as _;

use super::{
    Setup, WorkspaceSetting,
    error::Error,
    ordering::MonitorOrdering,
    output::{
        ActiveState as OutputActiveState, Class as OutputClass, Height as OutputHeight,
        Mode as OutputMode, Name as OutputName, Output, PosX, PosY, Position as OutputPosition,
        RefreshRate as OutputRefreshRate, Width as OutputWidth,
    },
    plan::Plan,
    workspace::{Name as WorkspaceName, Number as WorkspaceNumber, Workspace, Workspaces},
    workstation::{UnorderedWorkstation, Workstation},
};

enum PlanExpect<'ws, 'out> {
    Error,
    Valid { plan: Plan<'ws, 'out> },
}

static START: Once = Once::new();

fn setup() {
    if cfg!(feature = "testing-debug") {
        START.call_once(|| {
            tracing_subscriber::fmt()
                .with_max_level(Level::DEBUG)
                .init();
        });
    }
}

#[tokio::test]
async fn single_laptop() -> Result<(), Error> {
    setup();

    let mut connection = sway::tests::MockConnection {
        fail: false,
        setting: sway::tests::MockSetting::LaptopOnly,
    };

    let outputs = connection
        .outputs()
        .await?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?
        .tap_mut(|outputs| outputs.sort_by_key(|output| output.name.as_str().to_owned()));

    let workspaces = Workspaces::convert(
        connection.workspaces().await?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation =
        UnorderedWorkstation::from_outputs(&outputs)?.order(&MonitorOrdering::Default);

    assert_eq!(
        &workstation,
        &Workstation::LaptopOnly {
            laptop: &outputs[0]
        }
    );

    for (setup, expect) in [
        (Setup::LaptopLeft, PlanExpect::Error),
        (Setup::LaptopRight, PlanExpect::Error),
        (
            Setup::LaptopOnly,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![outputs[0].set(OutputActiveState::On {
                        position: OutputPosition::null(),
                    })],
                    workspace_settings: vec![
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(4),
                                name: WorkspaceName::try_new("num4".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                    ],
                    setup: Setup::LaptopOnly,
                },
            },
        ),
        (Setup::MonitorOnly, PlanExpect::Error),
    ] {
        let result = workstation.plan(setup, &workspaces);
        match expect {
            PlanExpect::Error => assert!(result.is_err()),
            PlanExpect::Valid { plan } => {
                assert_eq!(result?, plan);
            }
        }
    }

    Ok(())
}

#[test]
fn multiple_laptops() {
    let laptop1 = Output {
        name: OutputName::try_new("eDP-1".to_owned()).unwrap(),
        mode: OutputMode {
            width: OutputWidth::new(1920),
            height: OutputHeight::new(0),
            refresh_rate: OutputRefreshRate::Ms(144),
        },
        class: OutputClass::Laptop,
        state: OutputActiveState::On {
            position: OutputPosition {
                x: PosX(0),
                y: PosY(0),
            },
        },
    };

    let laptop2 = Output {
        name: OutputName::try_new("eDP-2".to_owned()).unwrap(),
        mode: OutputMode {
            width: OutputWidth::new(1920),
            height: OutputHeight::new(0),
            refresh_rate: OutputRefreshRate::Ms(144),
        },
        class: OutputClass::Laptop,
        state: OutputActiveState::On {
            position: OutputPosition {
                x: PosX(0),
                y: PosY(0),
            },
        },
    };

    let outputs = [laptop1, laptop2];

    let _workstation = UnorderedWorkstation::from_outputs(&outputs).unwrap_err();
}

#[test]
fn no_screens() {
    let outputs = [];

    let _workstation = UnorderedWorkstation::from_outputs(&outputs).unwrap_err();
}

#[tokio::test]
async fn single_monitor() -> Result<(), Error> {
    setup();

    let mut connection = sway::tests::MockConnection {
        fail: false,
        setting: sway::tests::MockSetting::MonitorOnly(1),
    };

    let outputs = connection
        .outputs()
        .await?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?
        .tap_mut(|outputs| outputs.sort_by_key(|output| output.name.as_str().to_owned()));

    let workspaces = Workspaces::convert(
        connection.workspaces().await?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation =
        UnorderedWorkstation::from_outputs(&outputs)?.order(&MonitorOrdering::Default);

    assert_eq!(
        workstation,
        Workstation::MonitorOnly {
            monitors: NonEmptyVec::new(outputs.iter().take(1).collect()).unwrap()
        }
    );

    for (setup, expect) in [
        (Setup::LaptopLeft, PlanExpect::Error),
        (Setup::LaptopRight, PlanExpect::Error),
        (Setup::LaptopOnly, PlanExpect::Error),
        (
            Setup::MonitorOnly,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![outputs[0].set(OutputActiveState::On {
                        position: OutputPosition::null(),
                    })],
                    workspace_settings: vec![
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(4),
                                name: WorkspaceName::try_new("num4".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                    ],
                    setup: Setup::MonitorOnly,
                },
            },
        ),
    ] {
        let result = workstation.plan(setup, &workspaces);
        match expect {
            PlanExpect::Error => assert!(result.is_err()),
            PlanExpect::Valid { plan } => {
                assert_eq!(result?, plan);
            }
        }
    }

    Ok(())
}

#[tokio::test]
async fn multiple_monitor() -> Result<(), Error> {
    setup();

    let mut connection = sway::tests::MockConnection {
        fail: false,
        setting: sway::tests::MockSetting::MonitorOnly(2),
    };

    let outputs = connection
        .outputs()
        .await?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?
        .tap_mut(|outputs| outputs.sort_by_key(|output| output.name.as_str().to_owned()));

    let workspaces = Workspaces::convert(
        connection.workspaces().await?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation =
        UnorderedWorkstation::from_outputs(&outputs)?.order(&MonitorOrdering::Default);

    assert_eq!(
        workstation,
        Workstation::MonitorOnly {
            monitors: NonEmptyVec::new(outputs.iter().take(2).collect()).unwrap()
        }
    );

    for (setup, expect) in [
        (Setup::LaptopLeft, PlanExpect::Error),
        (Setup::LaptopRight, PlanExpect::Error),
        (Setup::LaptopOnly, PlanExpect::Error),
        (
            Setup::MonitorOnly,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        outputs[0].set(OutputActiveState::On {
                            position: OutputPosition::null(),
                        }),
                        outputs[1].set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(1920),
                                y: PosY(1080 - 800),
                            },
                        }),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: &outputs[1],
                            },
                            output: &outputs[0],
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: &outputs[1],
                            },
                        },
                    ],
                    setup: Setup::MonitorOnly,
                },
            },
        ),
    ] {
        let result = workstation.plan(setup, &workspaces);
        match expect {
            PlanExpect::Error => assert!(result.is_err()),
            PlanExpect::Valid { plan } => {
                assert_eq!(result?, plan);
            }
        }
    }

    Ok(())
}

#[tokio::test]
async fn multiple_monitor_with_custom_order() -> Result<(), Error> {
    setup();

    let mut connection = sway::tests::MockConnection {
        fail: false,
        setting: sway::tests::MockSetting::MonitorOnly(2),
    };

    let outputs = connection
        .outputs()
        .await?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?
        .tap_mut(|outputs| outputs.sort_by_key(|output| output.name.as_str().to_owned()));

    let workspaces = Workspaces::convert(
        connection.workspaces().await?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation = UnorderedWorkstation::from_outputs(&outputs)?
        .order(&MonitorOrdering::Custom { order: vec![2, 1] });

    for (setup, expect) in [
        (Setup::LaptopLeft, PlanExpect::Error),
        (Setup::LaptopRight, PlanExpect::Error),
        (Setup::LaptopOnly, PlanExpect::Error),
        (
            Setup::MonitorOnly,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        outputs[1].set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(0),
                                y: PosY(1080 - 800),
                            },
                        }),
                        outputs[0].set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(1024),
                                y: PosY(0),
                            },
                        }),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                            output: &outputs[1],
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: &outputs[0],
                            },
                            output: &outputs[1],
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: &outputs[1],
                            },
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: &outputs[1],
                            },
                            output: &outputs[0],
                        },
                    ],
                    setup: Setup::MonitorOnly,
                },
            },
        ),
    ] {
        let result = workstation.plan(setup, &workspaces);
        match expect {
            PlanExpect::Error => assert!(result.is_err()),
            PlanExpect::Valid { plan } => {
                assert_eq!(result?, plan);
            }
        }
    }

    Ok(())
}

#[tokio::test]
async fn mixture() -> Result<(), Error> {
    setup();

    let mut connection = sway::tests::MockConnection {
        fail: false,
        setting: sway::tests::MockSetting::Mixed,
    };

    let mut outputs = connection
        .outputs()
        .await?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?;

    outputs.sort_by_key(|output| output.name.as_str().to_owned());

    let workspaces = Workspaces::convert(
        connection.workspaces().await?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation =
        UnorderedWorkstation::from_outputs(&outputs)?.order(&MonitorOrdering::Default);

    assert_eq!(
        workstation,
        Workstation::Mixed {
            laptop: outputs
                .iter()
                .find(|output| output.class == OutputClass::Laptop)
                .unwrap(),
            monitors: NonEmptyVec::new(
                outputs
                    .iter()
                    .filter(|output| output.class == OutputClass::Monitor)
                    .collect()
            )
            .unwrap()
        }
    );

    let Workstation::Mixed {
        laptop,
        ref monitors,
    } = workstation
    else {
        panic!()
    };

    for (setup, expect) in [
        (
            Setup::LaptopLeft,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        laptop.set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(0),
                                y: PosY(1440 - 1080),
                            },
                        }),
                        monitors.get(0).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(1920),
                                y: PosY(1440 - 800),
                            },
                        }),
                        monitors.get(1).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(1920 + 1024),
                                y: PosY(0),
                            },
                        }),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: &outputs[0],
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: &outputs[1],
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: monitors.get(0).unwrap(),
                            },
                            output: monitors.get(1).unwrap(),
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: monitors.get(1).unwrap(),
                            },
                            output: laptop,
                        },
                    ],
                    setup: Setup::LaptopLeft,
                },
            },
        ),
        (
            Setup::LaptopRight,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        monitors.get(0).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(0),
                                y: PosY(1440 - 800),
                            },
                        }),
                        monitors.get(1).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(1024),
                                y: PosY(0),
                            },
                        }),
                        laptop.set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(1024 + 2560),
                                y: PosY(1440 - 1080),
                            },
                        }),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: &outputs[0],
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: &outputs[1],
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: monitors.get(0).unwrap(),
                            },
                            output: monitors.get(1).unwrap(),
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: monitors.get(1).unwrap(),
                            },
                            output: laptop,
                        },
                    ],
                    setup: Setup::LaptopRight,
                },
            },
        ),
        (
            Setup::LaptopOnly,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        laptop.set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(0),
                                y: PosY(0),
                            },
                        }),
                        monitors.get(0).unwrap().set(OutputActiveState::Off),
                        monitors.get(1).unwrap().set(OutputActiveState::Off),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: laptop,
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: laptop,
                            },
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: monitors.get(0).unwrap(),
                            },
                            output: laptop,
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: monitors.get(1).unwrap(),
                            },
                            output: laptop,
                        },
                    ],
                    setup: Setup::LaptopOnly,
                },
            },
        ),
        (
            Setup::MonitorOnly,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        monitors.get(0).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(0),
                                y: PosY(1440 - 800),
                            },
                        }),
                        monitors.get(1).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(1024),
                                y: PosY(0),
                            },
                        }),
                        laptop.set(OutputActiveState::Off),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: monitors.get(0).unwrap(),
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: monitors.get(0).unwrap(),
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: monitors.get(0).unwrap(),
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: monitors.get(1).unwrap(),
                            },
                        },
                    ],
                    setup: Setup::MonitorOnly,
                },
            },
        ),
    ] {
        let result = workstation.plan(setup, &workspaces);
        match expect {
            PlanExpect::Error => assert!(result.is_err()),
            PlanExpect::Valid { plan } => {
                assert_eq!(result?, plan);
            }
        }
    }

    Ok(())
}

#[tokio::test]
async fn mixture_with_custom_order() -> Result<(), Error> {
    setup();

    let mut connection = sway::tests::MockConnection {
        fail: false,
        setting: sway::tests::MockSetting::Mixed,
    };

    let mut outputs = connection
        .outputs()
        .await?
        .into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<Output>, Error>>()?;

    outputs.sort_by_key(|output| output.name.as_str().to_owned());

    let workspaces = Workspaces::convert(
        connection.workspaces().await?,
        &outputs.iter().collect::<Vec<&Output>>(),
    )?;

    let workstation = UnorderedWorkstation::from_outputs(&outputs)?
        .order(&MonitorOrdering::Custom { order: vec![2, 1] });

    assert_eq!(
        workstation,
        Workstation::Mixed {
            laptop: outputs
                .iter()
                .find(|output| output.class == OutputClass::Laptop)
                .unwrap(),
            monitors: NonEmptyVec::new(
                outputs
                    .iter()
                    .filter(|output| output.class == OutputClass::Monitor)
                    .rev()
                    .collect()
            )
            .unwrap()
        }
    );

    let Workstation::Mixed {
        laptop,
        ref monitors,
    } = workstation
    else {
        panic!()
    };

    for (setup, expect) in [
        (
            Setup::LaptopLeft,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        laptop.set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(0),
                                y: PosY(1440 - 1080),
                            },
                        }),
                        monitors.get(0).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(1920),
                                y: PosY(0),
                            },
                        }),
                        monitors.get(1).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(1920 + 2560),
                                y: PosY(1440 - 800),
                            },
                        }),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: &outputs[1],
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: &outputs[0],
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: monitors.get(1).unwrap(),
                            },
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: monitors.get(0).unwrap(),
                            },
                            output: laptop,
                        },
                    ],
                    setup: Setup::LaptopLeft,
                },
            },
        ),
        (
            Setup::LaptopRight,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        monitors.get(0).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(0),
                                y: PosY(0),
                            },
                        }),
                        monitors.get(1).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(2560),
                                y: PosY(1440 - 800),
                            },
                        }),
                        laptop.set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(2560 + 1024),
                                y: PosY(1440 - 1080),
                            },
                        }),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: &outputs[1],
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: &outputs[0],
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: monitors.get(1).unwrap(),
                            },
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: monitors.get(0).unwrap(),
                            },
                            output: laptop,
                        },
                    ],
                    setup: Setup::LaptopRight,
                },
            },
        ),
        (
            Setup::LaptopOnly,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        laptop.set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(0),
                                y: PosY(0),
                            },
                        }),
                        monitors.get(0).unwrap().set(OutputActiveState::Off),
                        monitors.get(1).unwrap().set(OutputActiveState::Off),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: laptop,
                            },
                        },
                        WorkspaceSetting::Unchanged {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: laptop,
                            },
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: monitors.get(1).unwrap(),
                            },
                            output: laptop,
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: monitors.get(0).unwrap(),
                            },
                            output: laptop,
                        },
                    ],
                    setup: Setup::LaptopOnly,
                },
            },
        ),
        (
            Setup::MonitorOnly,
            PlanExpect::Valid {
                plan: Plan {
                    output_settings: vec![
                        monitors.get(0).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(0),
                                y: PosY(0),
                            },
                        }),
                        monitors.get(1).unwrap().set(OutputActiveState::On {
                            position: OutputPosition {
                                x: PosX(2560),
                                y: PosY(1440 - 800),
                            },
                        }),
                        laptop.set(OutputActiveState::Off),
                    ],
                    workspace_settings: vec![
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(1),
                                name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: monitors.get(0).unwrap(),
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(2),
                                name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                                output: laptop,
                            },
                            output: monitors.get(0).unwrap(),
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(3),
                                name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                                output: monitors.get(1).unwrap(),
                            },
                            output: monitors.get(0).unwrap(),
                        },
                        WorkspaceSetting::Change {
                            workspace: &Workspace {
                                num: WorkspaceNumber::new(10),
                                name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                                output: monitors.get(0).unwrap(),
                            },
                            output: monitors.get(1).unwrap(),
                        },
                    ],
                    setup: Setup::MonitorOnly,
                },
            },
        ),
    ] {
        let result = workstation.plan(setup, &workspaces);
        match expect {
            PlanExpect::Error => assert!(result.is_err()),
            PlanExpect::Valid { plan } => {
                assert_eq!(result?, plan);
            }
        }
    }

    Ok(())
}
