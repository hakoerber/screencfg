use std::{fmt, iter};

use nonempty::NonEmptyVec;
use sway::Conn as _;
use thiserror::Error;

use super::{
    OutputActiveState, OutputSetting, Setup, WorkspaceSetting, Workstation, output, output::Output,
    workspace::Number as WorkspaceNumber, workspace::Workspaces,
};

#[derive(Debug, Error)]
pub(crate) enum Error {
    #[error("no monitor outputs found")]
    NoMonitors,
    #[error("no laptop output found")]
    NoLaptop,
    #[error("more than 2 monitor monitors not supported")]
    MoreThanTwoMonitors,
    #[error("only workspaces between 1 and 10 are supported, found {number}")]
    WorkspaceNumber { number: usize },
    #[error("no monitor screen for projector found")]
    NoMonitorForProjector,
    #[error("no projector possible without laptop")]
    NoLaptopForProjector,
    #[error("can only project with single monitor screen, {count} found")]
    ProjectWithMultipleScreens { count: usize },
}

#[derive(Debug, Error)]
pub(crate) enum ApplyError {
    #[error("sway error: {0}")]
    Sway(#[from] sway::Error),
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) struct Plan<'ws, 'out> {
    pub(crate) output_settings: Vec<OutputSetting<'out>>,
    pub(crate) workspace_settings: Vec<WorkspaceSetting<'ws, 'out>>,
    pub(crate) setup: Setup,
}

impl fmt::Display for Plan<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, output) in self.output_settings.iter().enumerate() {
            let last = i
                == self
                    .output_settings
                    .len()
                    .checked_sub(1)
                    .expect("len is nonzero");
            if last {
                if self.workspace_settings.is_empty() {
                    write!(f, "{output}")?;
                } else {
                    writeln!(f, "{output}")?;
                }
            } else {
                writeln!(f, "{output}")?;
            }
        }

        for (i, workspace) in self.workspace_settings.iter().enumerate() {
            let last = i
                == self
                    .workspace_settings
                    .len()
                    .checked_sub(1)
                    .expect("len is nonzero");
            if last {
                write!(f, "{workspace}")?;
            } else {
                writeln!(f, "{workspace}")?;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) enum Command<'out> {
    SetOutputState {
        output: &'out Output,
        state: OutputActiveState,
    },
    MoveWorkspace {
        num: WorkspaceNumber,
        output: &'out Output,
    },
}

impl fmt::Display for Command<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::SetOutputState { output, ref state } => {
                write!(f, "setting output {output} to {state}")
            }
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
            .filter(|setting| match **setting {
                OutputSetting::Unchanged { output } => output.state.is_active(),
                OutputSetting::Change { output: _, state } => state.is_active(),
            })
            .collect::<Vec<_>>();

        let padding_top = active_outputs
            .iter()
            .map(|setting| "─".repeat(setting.output().name.len()))
            .collect::<Vec<_>>()
            .join("─┬─");

        let padding_bottom = active_outputs
            .iter()
            .map(|setting| "─".repeat(setting.output().name.len()))
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
                .map(|setting| setting.output().name.as_str())
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

    pub(crate) fn commands(&'out self) -> Vec<Command<'out>> {
        let mut commands = vec![];

        for setting in &self.output_settings {
            match *setting {
                OutputSetting::Unchanged { output: _ } => (),
                OutputSetting::Change { output, state } => {
                    commands.push(Command::SetOutputState { output, state });
                }
            }
        }

        for setting in &self.workspace_settings {
            match *setting {
                WorkspaceSetting::Unchanged { workspace: _ } => (),
                WorkspaceSetting::Change { workspace, output } => {
                    let from = workspace.output;
                    let to = output;

                    assert_ne!(
                        from.name, to.name,
                        "moving workspace to its current location, logic error"
                    );

                    commands.push(Command::MoveWorkspace {
                        num: workspace.num,
                        output: to,
                    });
                }
            }
        }

        commands
    }

    pub(crate) async fn apply(
        sway: &mut sway::Connection,
        commands: Vec<Command<'out>>,
    ) -> Result<(), ApplyError> {
        // we need to do the output commands first, as sway may create new workspaces automatically on newly
        // created outputs.

        pub(crate) enum Either<A, B> {
            A(A),
            B(B),
        }

        pub(crate) struct SetOutputState<'out> {
            output: &'out Output,
            state: OutputActiveState,
        }

        pub(crate) struct MoveWorkspace<'out> {
            num: WorkspaceNumber,
            output: &'out Output,
        }

        let (output_commands, workspace_commands) = commands
            .into_iter()
            .map(|command| match command {
                Command::SetOutputState { output, state } => {
                    Either::A(SetOutputState { output, state })
                }
                Command::MoveWorkspace { num, output } => Either::B(MoveWorkspace { num, output }),
            })
            .fold(
                (Vec::new(), Vec::new()),
                |mut vecs: (Vec<SetOutputState<'_>>, Vec<MoveWorkspace<'_>>), r| match r {
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

        for command in output_commands {
            match command.state {
                OutputActiveState::On { position } => {
                    sway.command(sway::Command::Output {
                        output_name: (&command.output.name).into(),
                        state: sway::OutputState::Enabled {
                            refresh_rate: command.output.mode.refresh_rate.into(),
                            position: position.into(),
                        },
                    })
                    .await?;
                }
                OutputActiveState::Off => {
                    sway.command(sway::Command::Output {
                        output_name: (&command.output.name).into(),
                        state: sway::OutputState::Disabled,
                    })
                    .await?;
                }
            }
        }

        for command in workspace_commands {
            sway.command(sway::Command::MoveWorkspace {
                number: command.num.into(),
                output_name: (&command.output.name).into(),
            })
            .await?;
        }

        Ok(())
    }
}

impl<'ws, 'out> Workstation<'out> {
    pub(crate) fn plan(
        &self,
        setup: Setup,
        workspaces: &'ws Workspaces<'out>,
    ) -> Result<Plan<'ws, 'out>, Error> {
        match *self {
            Self::LaptopOnly { laptop } => match setup {
                Setup::LaptopLeft | Setup::LaptopRight | Setup::MonitorOnly => {
                    Err(Error::NoMonitors)
                }
                Setup::LaptopOnly => Ok(Self::all_on_laptop(workspaces, laptop, None, setup)),
                Setup::Projector => Err(Error::NoMonitorForProjector),
            },
            Self::MonitorOnly { ref monitors } => match setup {
                Setup::LaptopLeft | Setup::LaptopRight | Setup::LaptopOnly => Err(Error::NoLaptop),
                Setup::MonitorOnly => Self::all_on_monitor(
                    workspaces,
                    None,
                    monitors,
                    self.max_output_height(),
                    setup,
                ),
                Setup::Projector => Err(Error::NoLaptopForProjector),
            },
            Self::Mixed {
                laptop,
                ref monitors,
            } => match setup {
                Setup::LaptopLeft => {
                    let max_output_height = self.max_output_height();

                    let workspace_settings =
                        Self::distribute_workspaces(workspaces, laptop, monitors)?;

                    // keeping track of horizontal position, from left to right
                    let mut pos_x_cursor = output::PosX(0);

                    let output_settings = iter::once(&laptop)
                        .chain(monitors)
                        .map(|output| {
                            let out = output.set(OutputActiveState::On {
                                position: {
                                    output::Position {
                                        x: pos_x_cursor,
                                        y: max_output_height
                                            .sub(output.mode.height)
                                            .end()
                                            .expect("always positive, as max_height >= height"),
                                    }
                                },
                            });

                            // advance cursor to the right
                            pos_x_cursor.add(output::PosX(output.mode.width.into_inner()));
                            out
                        })
                        .collect();

                    Ok(Plan {
                        output_settings,
                        workspace_settings,
                        setup,
                    })
                }
                Setup::LaptopRight => {
                    let max_output_height = self.max_output_height();

                    // keeping track of horizontal position, from left to right
                    let mut pos_x_cursor = output::PosX(0);

                    let workspace_settings =
                        Self::distribute_workspaces(workspaces, laptop, monitors)?;

                    let output_settings = (monitors)
                        .into_iter()
                        .chain(iter::once(&laptop))
                        .map(|output| {
                            let out = output.set(OutputActiveState::On {
                                position: {
                                    output::Position {
                                        x: pos_x_cursor,
                                        y: max_output_height
                                            .sub(output.mode.height)
                                            .end()
                                            .expect("always positive, as max_height >= height"),
                                    }
                                },
                            });

                            // advance cursor to the right
                            pos_x_cursor.add(output::PosX(output.mode.width.into_inner()));
                            out
                        })
                        .collect();

                    Ok(Plan {
                        output_settings,
                        workspace_settings,
                        setup,
                    })
                }
                Setup::LaptopOnly => Ok(Self::all_on_laptop(
                    workspaces,
                    laptop,
                    Some(monitors),
                    setup,
                )),
                Setup::MonitorOnly => Self::all_on_monitor(
                    workspaces,
                    Some(laptop),
                    monitors,
                    self.max_output_height(),
                    setup,
                ),
                Setup::Projector => match monitors.len() {
                    1 => Self::projector(workspaces, laptop, monitors.first(), setup),
                    count => Err(Error::ProjectWithMultipleScreens { count }),
                },
            },
        }
    }

    fn all_on_laptop(
        workspaces: &'ws Workspaces<'out>,
        laptop: &'out Output,
        monitors: Option<&NonEmptyVec<&'out Output>>,
        setup: Setup,
    ) -> Plan<'ws, 'out> {
        let output_settings = {
            let mut outputs = vec![laptop.set(OutputActiveState::On {
                position: output::Position::null(),
            })];
            outputs.append(&mut match monitors {
                None => vec![],
                Some(monitors) => monitors
                    .iter()
                    .map(|ext| ext.set(OutputActiveState::Off))
                    .collect(),
            });
            outputs
        };

        let workspace_settings = workspaces
            .0
            .iter()
            .map(|workspace| {
                if workspace.output.name == laptop.name {
                    WorkspaceSetting::Unchanged { workspace }
                } else {
                    WorkspaceSetting::Change {
                        workspace,
                        output: laptop,
                    }
                }
            })
            .collect();

        Plan {
            output_settings,
            workspace_settings,
            setup,
        }
    }

    fn all_on_monitor(
        workspaces: &'ws Workspaces<'out>,
        laptop: Option<&'out Output>,
        monitors: &NonEmptyVec<&'out Output>,
        max_output_height: output::Height,
        setup: Setup,
    ) -> Result<Plan<'ws, 'out>, Error> {
        Ok(Plan {
            output_settings: {
                // keeping track of horizontal position, from left to right
                let mut pos_x_cursor = output::PosX(0);

                let mut outputs: Vec<_> = monitors
                    .iter()
                    .map(|output| {
                        let out = output.set(OutputActiveState::On {
                            position: output::Position {
                                x: pos_x_cursor,
                                y: max_output_height
                                    .sub(output.mode.height)
                                    .end()
                                    .expect("always positive, as max_height >= height"),
                            },
                        });

                        // advance cursor to the right
                        pos_x_cursor.add(output::PosX(output.mode.width.into_inner()));
                        out
                    })
                    .collect();

                if let Some(laptop) = laptop {
                    outputs.push(laptop.set(OutputActiveState::Off));
                }
                outputs
            },
            workspace_settings: {
                let mut v = vec![];
                for workspace in &workspaces.0 {
                    let target_output = match workspace.num.into_inner() {
                        1..=5 => *monitors.first(),
                        6..=10 => match monitors.len() {
                            1 => *monitors.first(),
                            2 => monitors.get(1).expect("checked len above"),
                            _ => {
                                return Err(Error::MoreThanTwoMonitors);
                            }
                        },
                        number => {
                            return Err(Error::WorkspaceNumber { number });
                        }
                    };
                    {
                        v.push(if workspace.output.name == target_output.name {
                            WorkspaceSetting::Unchanged { workspace }
                        } else {
                            WorkspaceSetting::Change {
                                workspace,
                                output: target_output,
                            }
                        });
                    }
                }
                v
            },
            setup,
        })
    }

    fn projector(
        workspaces: &'ws Workspaces<'out>,
        laptop: &'out Output,
        monitor: &'out Output,
        setup: Setup,
    ) -> Result<Plan<'ws, 'out>, Error> {
        let mut workspace_settings = vec![];
        for workspace in &workspaces.0 {
            let target_output = match workspace.num.into_inner() {
                0..=9 => laptop,
                10 => monitor,
                number => {
                    return Err(Error::WorkspaceNumber { number });
                }
            };
            workspace_settings.push(if workspace.output.name == target_output.name {
                WorkspaceSetting::Unchanged { workspace }
            } else {
                WorkspaceSetting::Change {
                    workspace,
                    output: target_output,
                }
            });
        }

        let mut output_settings = vec![monitor.set(OutputActiveState::On {
            position: output::Position::null(),
        })];

        // put laptop below projector output, aligned left
        output_settings.push(laptop.set(OutputActiveState::On {
            position: output::Position {
                x: output::PosX(0),
                y: output::PosY(monitor.mode.height.into_inner()),
            },
        }));

        Ok(Plan {
            output_settings,
            workspace_settings,
            setup,
        })
    }

    fn distribute_workspaces(
        workspaces: &'ws Workspaces<'out>,
        laptop: &'out Output,
        monitors: &NonEmptyVec<&'out Output>,
    ) -> Result<Vec<WorkspaceSetting<'ws, 'out>>, Error> {
        let mut v = vec![];
        for workspace in &workspaces.0 {
            let target_output = match workspace.num.into_inner() {
                7..=10 => laptop,
                i @ 1..=6 => match monitors.len() {
                    1 => monitors.first(),
                    2 => match i {
                        1 => monitors.first(),
                        2..=6 => monitors.get(1).expect("checked the range above"),
                        _ => unreachable!("checked the range above"),
                    },
                    _ => {
                        return Err(Error::MoreThanTwoMonitors);
                    }
                },
                number => {
                    return Err(Error::WorkspaceNumber { number });
                }
            };
            v.push(if workspace.output.name == target_output.name {
                WorkspaceSetting::Unchanged { workspace }
            } else {
                WorkspaceSetting::Change {
                    workspace,
                    output: target_output,
                }
            });
        }
        Ok(v)
    }
}
