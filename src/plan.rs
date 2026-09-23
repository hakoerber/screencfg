use i3::Conn as _;

use std::ffi::OsStr;
use std::fmt;
use std::process;

use super::{
    OutputActiveState, OutputSetting, OutputState, Setup, Workstation, error::Error,
    nonempty::NonEmptyVec, ordering::ExternalOrdering, output::Output, workspace::WorkspaceNumber,
    workspace::WorkspaceSetting, workspace::Workspaces,
};

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Plan<'ws, 'out> {
    pub(crate) output_settings: Vec<OutputSetting<'out>>,
    pub(crate) workspace_settings: Vec<WorkspaceSetting<'ws, 'out>>,
}

impl fmt::Display for Plan<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for output in &self.output_settings {
            writeln!(f, "{output}")?;
        }
        for workspace in &self.workspace_settings {
            writeln!(f, "{workspace}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct Program(&'static str);

impl AsRef<OsStr> for Program {
    fn as_ref(&self) -> &OsStr {
        self.0.as_ref()
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) struct Arg<'a>(&'a str);

impl AsRef<OsStr> for Arg<'_> {
    fn as_ref(&self) -> &OsStr {
        self.0.as_ref()
    }
}

impl Arg<'_> {
    pub(crate) fn as_str(&self) -> &str {
        self.0
    }
}

#[derive(Debug)]
pub(crate) enum Command<'out, 'args> {
    Xrandr {
        program: Program,
        args: Vec<Arg<'args>>,
    },
    MoveWorkspace {
        num: WorkspaceNumber,
        output: &'out Output,
    },
}

impl fmt::Display for Command<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Xrandr {
                ref program,
                ref args,
            } => write!(
                f,
                "{} {}",
                program,
                args.iter()
                    .map(Arg::as_str)
                    .collect::<Vec<&str>>()
                    .join(" ")
            ),
            Self::MoveWorkspace { num, output } => {
                write!(f, "move workspace {num} to {}", output.name)
            }
        }
    }
}

impl<'out> Plan<'_, 'out> {
    pub(crate) fn diagram(&self, f: &mut impl fmt::Write) -> fmt::Result {
        let active_outputs = self
            .output_settings
            .iter()
            .filter(|setting| setting.state == OutputState::Connected(OutputActiveState::On))
            .collect::<Vec<_>>();

        let padding_top = active_outputs
            .iter()
            .map(|setting| "─".repeat(setting.output.name.len()))
            .collect::<Vec<_>>()
            .join("─┬─");

        let padding_bottom = active_outputs
            .iter()
            .map(|setting| "─".repeat(setting.output.name.len()))
            .collect::<Vec<_>>()
            .join("─┴─");

        writeln!(
            f,
            "┌{}┐\n┆ ╭─{padding_top}─╮ ┆",
            "┄".repeat(
                padding_top
                    .chars()
                    .count()
                    .checked_add((2 * 2) + 2)
                    .expect("width overflowed")
            )
        )?;
        writeln!(
            f,
            "┆ │ {} │ ┆",
            active_outputs
                .into_iter()
                .map(|setting| setting.output.name.as_str())
                .collect::<Vec<_>>()
                .join(" │ ")
        )?;
        write!(
            f,
            "┆ ╰─{padding_bottom}─╯ ┆\n└{}┘",
            "┄".repeat(
                padding_bottom
                    .chars()
                    .count()
                    .checked_add(2 * 2 + 2)
                    .expect("width overflowed")
            )
        )?;
        Ok(())
    }

    pub(crate) fn commands(&'out self) -> Vec<Command<'out, 'out>> {
        let mut commands = vec![];
        let mut args = vec![];

        let mut left: Option<&OutputSetting<'_>> = None;

        for setting in &self.output_settings {
            args.push(Arg("--output"));
            args.push(Arg(setting.output.name.as_str()));

            match setting.state {
                OutputState::Connected(OutputActiveState::On) => {
                    args.push(Arg("--auto"));
                    if let Some(left) = left {
                        args.push(Arg("--right-of"));
                        args.push(Arg(left.output.name.as_str()));
                    }
                    left = Some(setting);
                }
                OutputState::Connected(OutputActiveState::Off) | OutputState::Disconnected => {
                    args.push(Arg("--off"));
                }
            }
        }

        commands.push(Command::Xrandr {
            program: Program("xrandr"),
            args,
        });

        for setting in &self.workspace_settings {
            let from = &setting.workspace.output;
            let to = &setting.output;

            assert_ne!(
                from, to,
                "moving workspace to its current location, logic error"
            );

            commands.push(Command::MoveWorkspace {
                num: setting.workspace.num,
                output: to,
            });
        }

        commands
    }

    pub(crate) fn apply(
        i3: &mut i3::Connection,
        commands: Vec<Command<'out, '_>>,
    ) -> Result<(), Error> {
        // we need to do the xrandr commands first, as i3 may create new workspaces automatically on newly
        // created outputs.

        pub(crate) enum Either<A, B> {
            A(A),
            B(B),
        }

        pub(crate) struct XrandrCommand<'args> {
            program: Program,
            args: Vec<Arg<'args>>,
        }

        pub(crate) struct MoveWorkspace<'out> {
            num: WorkspaceNumber,
            output: &'out Output,
        }

        let (xrandr_commands, i3_commands) = commands
            .into_iter()
            .map(|command| match command {
                Command::Xrandr { program, args } => Either::A(XrandrCommand { program, args }),
                Command::MoveWorkspace { num, output } => Either::B(MoveWorkspace { num, output }),
            })
            .fold(
                (Vec::new(), Vec::new()),
                |mut vecs: (Vec<XrandrCommand<'_>>, Vec<MoveWorkspace<'_>>), r| match r {
                    Either::A(cmd) => {
                        vecs.0.push(cmd);
                        vecs
                    }
                    Either::B(cmd) => {
                        vecs.1.push(cmd);
                        vecs
                    }
                },
            );

        for command in xrandr_commands {
            let output = process::Command::new(command.program)
                .args(command.args)
                .output()
                .map_err(|e| Error::Command(e.to_string().into()))?;

            if !output.status.success() {
                return Err(Error::Apply(
                    String::from_utf8(output.stderr)
                        .map_err(|err| Error::Command(err.to_string().into()))?
                        .into(),
                ));
            }
        }

        // do an explicit i3 reload to allow i3 to pick up the new outputs
        i3.command(i3::Command::Reload)?;

        for command in i3_commands {
            i3.command(i3::Command::MoveWorkspace {
                number: command.num.into(),
                output_name: (&command.output.name).into(),
            })?;
        }

        Ok(())
    }
}

impl<'ws, 'out> Workstation<'out> {
    fn all_on_laptop(
        workspaces: &'ws Workspaces<'out>,
        laptop: &'out Output,
        externals: Option<&NonEmptyVec<&'out Output>>,
        disconnected_externals: &[&'out Output],
    ) -> Plan<'ws, 'out> {
        Plan {
            output_settings: {
                let mut outputs = vec![laptop.on()];
                outputs.append(&mut match externals {
                    None => vec![],
                    Some(externals) => externals.iter().map(|ext| ext.off()).collect(),
                });
                outputs.extend(disconnected_externals.iter().map(|output| output.off()));
                outputs
            },
            workspace_settings: workspaces
                .0
                .iter()
                .filter(|workspace| workspace.output != laptop)
                .map(|workspace| WorkspaceSetting {
                    workspace,
                    output: laptop,
                })
                .collect(),
        }
    }

    fn all_on_external(
        workspaces: &'ws Workspaces<'out>,
        laptop: Option<&'out Output>,
        externals: &NonEmptyVec<&'out Output>,
        disconnected_externals: &[&'out Output],
        external_ordering: &ExternalOrdering,
    ) -> Result<Plan<'ws, 'out>, Error> {
        // shuffle around if required
        let externals = match *external_ordering {
            ExternalOrdering::Default => externals,
            ExternalOrdering::Custom { ref order } => {
                let mut out = Vec::with_capacity(externals.len());
                assert_eq!(
                    order.len(),
                    externals.len(),
                    "assured during creation of order"
                );

                for order in order {
                    out.push(
                        *externals
                            .get(order.checked_sub(1).expect("order is always positive"))
                            .expect("order contains incrementing integers"),
                    );
                }

                &NonEmptyVec::new(out)
            }
        };

        Ok(Plan {
            output_settings: {
                let mut outputs: Vec<_> = externals.iter().map(|output| output.on()).collect();

                if let Some(laptop) = laptop {
                    outputs.push(laptop.off());
                }
                outputs.extend(disconnected_externals.iter().map(|output| output.off()));
                outputs
            },
            workspace_settings: {
                let mut v = vec![];
                for workspace in &workspaces.0 {
                    let target_output = match workspace.num.0 {
                        1..=5 => *externals.first(),
                        6..=10 => match externals.len() {
                            1 => *externals.first(),
                            2 => externals.get(1).expect("checked len above"),
                            _ => {
                                return Err(Error::InvalidSetup(
                                    "more than 2 external monitors not supported".into(),
                                ));
                            }
                        },
                        _ => {
                            return Err(Error::InvalidSetup(
                                "only workspaces between 1 and 10 are supported".into(),
                            ));
                        }
                    };
                    if workspace.output != target_output {
                        v.push(WorkspaceSetting {
                            workspace,
                            output: target_output,
                        });
                    }
                }
                v
            },
        })
    }

    fn projector(
        workspaces: &'ws Workspaces<'out>,
        laptop: &'out Output,
        external: &'out Output,
    ) -> Result<Vec<WorkspaceSetting<'ws, 'out>>, Error> {
        let mut v = vec![];
        for workspace in &workspaces.0 {
            let target_output = match workspace.num.0 {
                0..=9 => laptop,
                10 => external,
                _ => {
                    return Err(Error::InvalidSetup(
                        "only workspaces between 1 and 10 are supported".into(),
                    ));
                }
            };
            if workspace.output != target_output {
                v.push(WorkspaceSetting {
                    workspace,
                    output: target_output,
                });
            }
        }
        Ok(v)
    }

    fn distribute_workspaces(
        workspaces: &'ws Workspaces<'out>,
        laptop: &'out Output,
        externals: &NonEmptyVec<&'out Output>,
    ) -> Result<Vec<WorkspaceSetting<'ws, 'out>>, Error> {
        let mut v = vec![];
        for workspace in &workspaces.0 {
            let target_output = match workspace.num.0 {
                7..=10 => laptop,
                i @ 1..=6 => match externals.len() {
                    1 => externals.first(),
                    2 => match i {
                        1 => externals.first(),
                        2..=6 => externals.get(1).expect("checked the range above"),
                        _ => unreachable!("checked the range above"),
                    },
                    _ => {
                        return Err(Error::InvalidSetup(
                            "more than 2 external monitors not supported".into(),
                        ));
                    }
                },
                _ => {
                    return Err(Error::InvalidSetup(
                        "only workspaces between 1 and 10 are supported".into(),
                    ));
                }
            };
            if workspace.output != target_output {
                v.push(WorkspaceSetting {
                    workspace,
                    output: target_output,
                });
            }
        }
        Ok(v)
    }

    pub(crate) fn plan(
        &self,
        setup: Setup,
        workspaces: &'ws Workspaces<'out>,
        external_ordering: &ExternalOrdering,
    ) -> Result<Plan<'ws, 'out>, Error> {
        match setup {
            setup @ (Setup::LaptopLeft | Setup::LaptopRight) => match self.laptop {
                None => Err(Error::Plan("no laptop screen found".into())),
                Some(laptop) => {
                    let Some(ref externals) = self.externals else {
                        return Err(Error::Plan("no external screens found".into()));
                    };

                    // shuffle around if required
                    let externals = match *external_ordering {
                        ExternalOrdering::Default => externals,
                        ExternalOrdering::Custom { ref order } => {
                            let mut out = Vec::with_capacity(externals.len());
                            assert_eq!(
                                order.len(),
                                externals.len(),
                                "assured during creation of order"
                            );

                            for order in order {
                                out.push(
                                    *externals
                                        .get(
                                            order.checked_sub(1).expect("order is always positive"),
                                        )
                                        .expect("order contains incrementing integers"),
                                );
                            }

                            &NonEmptyVec::new(out)
                        }
                    };

                    let workspace_settings =
                        Self::distribute_workspaces(workspaces, laptop, externals)?;

                    let mut output_settings: Vec<OutputSetting<'_>> =
                        externals.into_iter().map(|ext| ext.on()).collect();

                    output_settings.extend(
                        self.disconnected_externals
                            .iter()
                            .map(|output| output.off()),
                    );

                    match setup {
                        Setup::LaptopLeft => output_settings.insert(0, laptop.on()),
                        Setup::LaptopRight => output_settings.push(laptop.on()),
                        Setup::LaptopOnly | Setup::ExternalOnly | Setup::Projector => {
                            unreachable!("checked for enum values above")
                        }
                    }

                    Ok(Plan {
                        output_settings,
                        workspace_settings,
                    })
                }
            },
            Setup::Projector => match self.laptop {
                None => Err(Error::Plan("no laptop screen found".into())),
                Some(laptop) => {
                    let Some(ref externals) = self.externals else {
                        return Err(Error::Plan("no external screens found".into()));
                    };

                    if externals.len() != 1 {
                        return Err(Error::Plan(
                            "can only project with single external screen".into(),
                        ));
                    }

                    let external = externals.first();

                    let workspace_settings = Self::projector(workspaces, laptop, external)?;

                    let mut output_settings = vec![laptop.on()];
                    output_settings.extend(externals.iter().map(|ext| ext.on()));
                    output_settings.extend(
                        self.disconnected_externals
                            .iter()
                            .map(|output| output.off()),
                    );

                    Ok(Plan {
                        output_settings,
                        workspace_settings,
                    })
                }
            },
            Setup::LaptopOnly => match self.laptop {
                None => Err(Error::Plan("no laptop screen found".into())),
                Some(laptop) => Ok(Self::all_on_laptop(
                    workspaces,
                    laptop,
                    self.externals.as_ref(),
                    &self.disconnected_externals,
                )),
            },
            Setup::ExternalOnly => match self.externals {
                None => Err(Error::Plan("no external screens found".into())),
                Some(ref externals) => Ok(Self::all_on_external(
                    workspaces,
                    self.laptop,
                    externals,
                    &self.disconnected_externals,
                    external_ordering,
                )?),
            },
        }
    }
}
