use std::fmt;
use std::io;
use std::process;
use std::string;

use clap::{Args, Parser, ValueEnum};

mod i3;
use i3::Conn;

#[derive(Debug)]
enum Error {
    Generic(String),
    Command(String),
    Classify(String),
    Workstation(String),
    Plan(String),
    Apply(String),
    I3(i3::Error),
    InvalidSetup(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::Generic(msg) => format!("error: {msg}"),
                Error::Command(msg) => format!("command failed: {msg}"),
                Error::Classify(msg) => format!("classification failed: {msg}"),
                Error::Workstation(msg) => format!("workstation failed: {msg}"),
                Error::Plan(msg) => format!("plan failed: {msg}"),
                Error::Apply(msg) => format!("apply failed: {msg}"),
                Error::I3(e) => format!("i3: {e}"),
                Error::InvalidSetup(msg) => format!("invalid setup: {msg}"),
            },
        )
    }
}

impl From<i3::Error> for Error {
    fn from(value: i3::Error) -> Self {
        Self::I3(value)
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Command(value.to_string())
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(value: string::FromUtf8Error) -> Self {
        Self::Command(value.to_string())
    }
}

impl std::error::Error for Error {}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum OutputClass {
    Laptop,
    External,
}

impl OutputClass {
    fn try_detect(value: &str) -> Result<Self, Error> {
        if value.starts_with("eDP-") {
            Ok(Self::Laptop)
        } else if value.starts_with("DP-") || value.starts_with("HDMI-") {
            Ok(Self::External)
        } else {
            Err(Error::Classify(format!(
                "could not classify output: {value}"
            )))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Output {
    class: OutputClass,
    name: String,
}

impl Output {
    fn on(self) -> OutputSetting {
        OutputSetting {
            output: self,
            state: OutputState::On,
        }
    }

    fn off(self) -> OutputSetting {
        OutputSetting {
            output: self,
            state: OutputState::Off,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Workstation {
    laptop: Option<Output>,
    externals: Option<(Output, Vec<Output>)>,
}

impl TryFrom<&[Output]> for Workstation {
    type Error = Error;

    fn try_from(value: &[Output]) -> Result<Self, Self::Error> {
        let (mut laptops, mut non_laptops): (Vec<_>, Vec<_>) = value
            .iter()
            .cloned()
            .partition(|output| output.class == OutputClass::Laptop);

        non_laptops.sort();

        let laptop = match laptops.len() {
            0 => None,
            1 => Some(laptops.remove(0)),
            _ => {
                return Err(Error::Workstation(
                    "found more than one laptop screen".into(),
                ))
            }
        };

        let (mut externals, rest): (Vec<_>, Vec<_>) = non_laptops
            .into_iter()
            .partition(|output| output.class == OutputClass::External);

        if laptop.is_none() && externals.is_empty() {
            return Err(Error::Workstation("no screens found".to_string()));
        }

        let externals = match externals.len() {
            0 => None,
            _ => Some((externals.remove(0), externals)),
        };

        assert_eq!(rest.len(), 0);

        Ok(Self { laptop, externals })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum OutputState {
    On,
    Off,
}

#[derive(Debug, PartialEq, Eq)]
struct OutputSetting {
    output: Output,
    state: OutputState,
}

#[derive(Debug)]
struct Workspaces(Vec<Workspace>);

impl Workspaces {
    fn convert(workspaces: i3::Workspaces, outputs: &[Output]) -> Result<Self, Error> {
        Ok(Self(
            workspaces
                .into_iter()
                .map(|from| {
                    Ok(Workspace {
                        num: from.num,
                        name: from.name,
                        output: outputs
                            .iter()
                            .find(|output| from.output == output.name)
                            .ok_or_else(|| {
                                Error::Generic(format!(
                                    "output of workspace {} ({}) not found in i3 outputs",
                                    from.num, from.output
                                ))
                            })?
                            .clone(),
                    })
                })
                .collect::<Result<Vec<Workspace>, Error>>()?,
        ))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Workspace {
    pub num: usize,
    #[allow(dead_code)]
    pub name: String,
    pub output: Output,
}

#[derive(Debug, PartialEq, Eq)]
struct WorkspaceSetting {
    workspace: Workspace,
    output: Output,
}

#[derive(Debug, PartialEq, Eq)]
struct Plan {
    output_settings: Vec<OutputSetting>,
    workspace_settings: Vec<WorkspaceSetting>,
}

#[derive(Debug)]
enum Command {
    Xrandr(String, Vec<String>),
    MoveWorkspace { num: usize, output: Output },
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Command::Xrandr(program, args) => write!(f, "{} {}", program, args.join(" ")),
            Command::MoveWorkspace { num, output } => {
                write!(f, "move workspace {num} to {}", output.name)
            }
        }
    }
}

impl Plan {
    fn commands(&self) -> Vec<Command> {
        let mut commands = vec![];
        let mut args = vec![];

        let mut left: Option<&OutputSetting> = None;

        for setting in &self.output_settings {
            args.push("--output".into());
            args.push(setting.output.name.clone());

            match setting.state {
                OutputState::On => {
                    args.push("--auto".into());
                    if let Some(left) = left {
                        args.push("--right-of".into());
                        args.push(left.output.name.clone());
                    }
                    left = Some(setting);
                }
                OutputState::Off => args.push("--off".into()),
            };
        }

        commands.push(Command::Xrandr("xrandr".into(), args));

        for setting in &self.workspace_settings {
            let from = &setting.workspace.output;
            let to = &setting.workspace.output;

            assert_ne!(from, to);

            commands.push(Command::MoveWorkspace {
                num: setting.workspace.num,
                output: to.clone(),
            });
        }

        commands
    }

    fn apply(self, i3: &mut i3::Connection) -> Result<Vec<Command>, Error> {
        let commands = self.commands();
        for command in &commands {
            match command {
                Command::Xrandr(program, args) => {
                    let output = process::Command::new(program).args(args).output()?;

                    let _ = output
                        .status
                        .success()
                        .then_some(())
                        .ok_or(Error::Apply(String::from_utf8(output.stderr)?));
                }
                Command::MoveWorkspace { num, output } => {
                    i3.command(i3::Command::MoveWorkspace {
                        id: *num,
                        output: output.name.clone(),
                    })?;
                }
            }
        }
        Ok(commands)
    }
}

impl fmt::Display for Plan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let active_outputs = self
            .output_settings
            .iter()
            .filter(|setting| setting.state == OutputState::On)
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
            "┄".repeat(padding_top.chars().count() + 2 * 2 + 2)
        )?;
        writeln!(
            f,
            "┆ │ {} │ ┆",
            active_outputs
                .into_iter()
                .map(|setting| setting.output.name.clone())
                .collect::<Vec<_>>()
                .join(" │ ")
        )?;
        write!(
            f,
            "┆ ╰─{padding_bottom}─╯ ┆\n└{}┘",
            "┄".repeat(padding_bottom.chars().count() + 2 * 2 + 2)
        )?;
        Ok(())
    }
}

impl Workstation {
    fn all_on_laptop(
        workspaces: &Workspaces,
        laptop: &Output,
        externals: Option<(Output, Vec<Output>)>,
    ) -> Plan {
        Plan {
            output_settings: {
                let mut v = vec![laptop.clone().on()];
                v.append({
                    &mut match externals {
                        None => vec![],
                        Some((ext, rest)) => {
                            let mut v = vec![ext.clone().off()];
                            v.append(&mut rest.iter().map(|ext| ext.clone().off()).collect());
                            v
                        }
                    }
                });
                v
            },
            workspace_settings: workspaces
                .0
                .iter()
                .filter(|workspace| (workspace.output != *laptop))
                .map(|workspace| WorkspaceSetting {
                    workspace: workspace.clone(),
                    output: laptop.clone(),
                })
                .collect(),
        }
    }

    fn all_on_external(
        workspaces: &Workspaces,
        laptop: Option<Output>,
        externals: &(Output, Vec<Output>),
    ) -> Result<Plan, Error> {
        Ok(Plan {
            output_settings: {
                let mut v = {
                    let mut v = vec![externals.0.clone().on()];
                    v.append(
                        &mut externals
                            .1
                            .iter()
                            .map(|output| output.clone().on())
                            .collect(),
                    );
                    v
                };
                if let Some(laptop) = laptop {
                    v.push(laptop.clone().off());
                }
                v
            },
            workspace_settings: {
                let mut v = vec![];
                for workspace in &workspaces.0 {
                    let target_output = match workspace.num {
                        1..=5 => externals.0.clone(),
                        6..=10 => match externals.1.len() {
                            0 => externals.0.clone(),
                            1 => externals.1.first().unwrap().clone(),
                            _ => {
                                return Err(Error::InvalidSetup(
                                    "more than 2 external monitors not supported".into(),
                                ))
                            }
                        },
                        _ => {
                            return Err(Error::InvalidSetup(
                                "only workspaces between 1 and 10 are supported".into(),
                            ))
                        }
                    };
                    if workspace.output != target_output {
                        v.push(WorkspaceSetting {
                            workspace: workspace.clone(),
                            output: target_output.clone(),
                        });
                    }
                }
                v
            },
        })
    }

    fn distribute_workspaces(
        workspaces: &Workspaces,
        laptop: &Output,
        externals: &(Output, Vec<Output>),
    ) -> Result<Vec<WorkspaceSetting>, Error> {
        let mut v = vec![];
        for workspace in &workspaces.0 {
            let target_output = match workspace.num {
                7..=10 => laptop.clone(),
                i @ 1..=6 => match externals.1.len() {
                    0 => externals.0.clone(),
                    1 => match i {
                        1..=3 => externals.0.clone(),
                        4..=6 => externals.1.first().unwrap().clone(),
                        _ => unreachable!(),
                    },
                    _ => {
                        return Err(Error::InvalidSetup(
                            "more than 2 external monitors not supported".into(),
                        ))
                    }
                },
                _ => {
                    return Err(Error::InvalidSetup(
                        "only workspaces between 1 and 10 are supported".into(),
                    ))
                }
            };
            if workspace.output != target_output {
                v.push(WorkspaceSetting {
                    workspace: workspace.clone(),
                    output: target_output,
                });
            }
        }
        Ok(v)
    }

    fn plan(&self, setup: Setup, workspaces: &Workspaces) -> Result<Plan, Error> {
        match setup {
            setup @ (Setup::LaptopLeft | Setup::LaptopRight) => match self.laptop {
                None => Err(Error::Plan("no laptop screen found".to_string())),
                Some(ref laptop) => {
                    let Some(ref externals) = self.externals else {
                        return Err(Error::Plan("no external screens found".to_string()));
                    };
                    let workspace_settings =
                        Self::distribute_workspaces(workspaces, laptop, externals)?;

                    let mut output_settings = vec![externals.0.clone().on()];
                    output_settings
                        .append(&mut externals.1.iter().map(|ext| ext.clone().on()).collect());

                    match setup {
                        Setup::LaptopLeft => output_settings.insert(0, laptop.clone().on()),
                        Setup::LaptopRight => output_settings.push(laptop.clone().on()),
                        _ => unreachable!(),
                    }

                    Ok(Plan {
                        output_settings,
                        workspace_settings,
                    })
                }
            },
            Setup::LaptopOnly => match self.laptop {
                None => Err(Error::Plan("no laptop screen found".to_string())),
                Some(ref laptop) => Ok(Self::all_on_laptop(
                    workspaces,
                    laptop,
                    self.externals.clone(),
                )),
            },
            Setup::ExternalOnly => match &self.externals {
                None => Err(Error::Plan("no external screens found".to_string())),
                Some(externals) => Ok(Self::all_on_external(
                    workspaces,
                    self.laptop.clone(),
                    externals,
                )?),
            },
        }
    }
}

impl Output {
    fn findall(i3: &mut i3::Connection) -> Result<Vec<Self>, Error> {
        i3.outputs()?
            .into_iter()
            .filter(|output| output.active)
            .map(TryInto::try_into)
            .collect::<Result<Vec<Output>, Error>>()
    }
}

impl TryFrom<i3::Output> for Output {
    type Error = Error;

    fn try_from(value: i3::Output) -> Result<Self, Self::Error> {
        let class = OutputClass::try_detect(&value.name)?;

        Ok(Self {
            name: value.name,
            class,
        })
    }
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Setup {
    LaptopLeft,
    LaptopRight,
    LaptopOnly,
    ExternalOnly,
}

#[derive(Clone, Debug, Args)]
#[group(multiple = false, required = true)]
struct Approach {
    #[arg(long)]
    setup: Option<Setup>,

    #[arg(long)]
    best: bool,
}

#[derive(Debug, Parser)]
#[command(version, about)]
struct Cli {
    #[command(flatten)]
    approach: Approach,

    #[arg(long)]
    dry_run: bool,

    #[arg(long)]
    diagram: bool,

    #[arg(long)]
    debug: bool,
}

#[allow(unreachable_code)]
fn run() -> Result<(), Error> {
    let args = Cli::parse();

    let mut i3_connection = i3::connect()?;

    let outputs = Output::findall(&mut i3_connection)?;

    if args.debug {
        println!("i3 outputs:");
        for output in &outputs {
            println!("  - {output:?}");
        }
        println!();
    }

    let workstation: Workstation = (&outputs[..]).try_into()?;

    let workspaces = i3_connection.workspaces()?;
    let workspaces = Workspaces::convert(workspaces, &outputs)?;

    if args.debug {
        println!("i3 workspaces:");
        for workspace in &workspaces.0 {
            println!("  - {workspace:?}");
        }
        println!();
    }

    i3_connection.command(i3::Command::Nop)?;

    let plan = if let Some(setup) = args.approach.setup {
        workstation.plan(setup, &workspaces)?
    } else {
        workstation
            .plan(Setup::LaptopLeft, &workspaces)
            .or_else(|_| workstation.plan(Setup::LaptopOnly, &workspaces))
            .or_else(|_| workstation.plan(Setup::ExternalOnly, &workspaces))
            .map_err(|_| Error::Plan("no plan fit with \"best\" strategy".to_string()))?
    };

    if args.debug {
        println!("{plan:?}");
        println!();
    }
    if args.diagram {
        println!("{plan}");
        println!();
    }

    let commands = if args.dry_run {
        plan.commands()
    } else {
        plan.apply(&mut i3_connection)?
    };

    println!("applying changes:");
    for command in commands {
        println!("- {command}");
    }

    Ok(())
}

fn main() -> process::ExitCode {
    process::ExitCode::from(match run() {
        Ok(()) => 0,
        Err(e) => {
            println!("{e}");
            1
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    enum PlanExpect<'a> {
        Error,
        Valid(Plan, &'a str),
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

        let workspaces = Workspaces::convert(connection.workspaces()?, &outputs)?;

        let workstation: Workstation = outputs[..].try_into()?;

        assert_eq!(
            &workstation,
            &Workstation {
                laptop: Some(outputs[0].clone()),
                externals: None
            }
        );

        for (setup, expect) in [
            (Setup::LaptopLeft, PlanExpect::Error),
            (Setup::LaptopRight, PlanExpect::Error),
            (
                Setup::LaptopOnly,
                PlanExpect::Valid(
                    Plan {
                        output_settings: vec![outputs[0].clone().on()],
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
        };

        let laptop2 = Output {
            name: "eDP-2".to_string(),
            class: OutputClass::Laptop,
        };

        let outputs = vec![laptop1, laptop2];

        let workstation: Result<Workstation, Error> = outputs[..].try_into();

        assert!(workstation.is_err());

        Ok(())
    }

    #[test]
    fn no_screens() -> Result<(), Error> {
        let outputs = vec![];

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

        let workspaces = Workspaces::convert(connection.workspaces()?, &outputs)?;

        let workstation: Workstation = outputs[..].try_into()?;

        assert_eq!(
            workstation,
            Workstation {
                laptop: None,
                externals: Some((outputs[0].clone(), vec![]))
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
                        output_settings: vec![outputs[0].clone().on()],
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

        let workspaces = Workspaces::convert(connection.workspaces()?, &outputs)?;

        let workstation: Workstation = outputs[..].try_into()?;

        assert_eq!(
            workstation,
            Workstation {
                laptop: None,
                externals: Some((outputs[0].clone(), vec![outputs[1].clone()]))
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
                        output_settings: vec![outputs[0].clone().on(), outputs[1].clone().on()],
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

        let workspaces = Workspaces::convert(connection.workspaces()?, &outputs)?;

        let workstation: Workstation = outputs[..].try_into()?;

        assert_eq!(
            workstation,
            Workstation {
                laptop: Some(outputs[0].clone()),
                externals: Some((outputs[1].clone(), vec![outputs[2].clone()]))
            }
        );

        for (setup, expect) in [
            (
                Setup::LaptopLeft,
                PlanExpect::Valid(
                    Plan {
                        output_settings: vec![
                            outputs[0].clone().on(),
                            outputs[1].clone().on(),
                            outputs[2].clone().on(),
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
                            outputs[1].clone().on(),
                            outputs[2].clone().on(),
                            outputs[0].clone().on(),
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
                            outputs[0].clone().on(),
                            outputs[1].clone().off(),
                            outputs[2].clone().off(),
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
                            outputs[1].clone().on(),
                            outputs[2].clone().on(),
                            outputs[0].clone().off(),
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
}
