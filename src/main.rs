use std::{
    collections::{hash_map::Entry, HashMap},
    env, fmt,
    path::PathBuf,
    process,
};

use clap::{Args, Parser, ValueEnum};

use i3::Conn as _;

mod error;
use error::Error;

mod config;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum OutputClass {
    Laptop,
    External,
}

impl fmt::Display for OutputClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Laptop => "Laptop",
                Self::External => "External",
            }
        )
    }
}

impl OutputClass {
    fn try_detect(value: &str) -> Result<Self, Error> {
        if value.starts_with("eDP-") {
            Ok(Self::Laptop)
        } else if value.starts_with("DP-")
            || value.starts_with("HDMI-")
            || value.starts_with("DisplayPort-")
        {
            Ok(Self::External)
        } else {
            Err(Error::Classify(
                format!("could not classify output: {value}").into(),
            ))
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum OutputConnectionState {
    Connected,
    Disconnected,
}

impl From<xrandr::OutputState> for OutputConnectionState {
    fn from(value: xrandr::OutputState) -> Self {
        match value {
            xrandr::OutputState::Connected => Self::Connected,
            xrandr::OutputState::Disconnected => Self::Disconnected,
        }
    }
}

impl fmt::Display for OutputConnectionState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                OutputConnectionState::Connected => "connected",
                OutputConnectionState::Disconnected => "disconnected",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Output {
    class: OutputClass,
    name: String,
    connection_state: OutputConnectionState,
}

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.class)
    }
}

impl<'out> Output {
    fn on(&'out self) -> OutputSetting<'out> {
        if self.connection_state != OutputConnectionState::Connected {
            panic!("tried to activate disconnected output")
        }
        OutputSetting {
            output: self,
            state: OutputState::Connected(OutputActiveState::On),
        }
    }

    fn off(&'out self) -> OutputSetting<'out> {
        let state = if self.connection_state == OutputConnectionState::Disconnected {
            OutputState::Disconnected
        } else {
            OutputState::Connected(OutputActiveState::Off)
        };

        OutputSetting {
            output: self,
            state,
        }
    }

    fn findall(i3: &mut i3::Connection) -> Result<Vec<Self>, Error> {
        let i3_outputs = i3
            .outputs()?
            .into_iter()
            .map(TryInto::try_into)
            .collect::<Result<Vec<Self>, Error>>()?;

        let xrandr_outputs = xrandr::Output::findall()?
            .into_iter()
            .map(TryInto::try_into)
            .collect::<Result<Vec<Self>, Error>>()?;

        // TODO: do this better, without cloning name

        let mut outputs: HashMap<String, Output> = HashMap::from_iter(
            i3_outputs
                .into_iter()
                .map(|output| (output.name.clone(), output)),
        );

        for xrandr_output in xrandr_outputs {
            match outputs.entry(xrandr_output.name.clone()) {
                Entry::Occupied(existing) => {
                    let i3_connection_state = &existing.get().connection_state;
                    if i3_connection_state != &xrandr_output.connection_state {
                        return Err(Error::Generic(
                            format!(
                                "connection state mismatch, i3:{}, xrandr:{}",
                                i3_connection_state, xrandr_output.connection_state,
                            )
                            .into(),
                        ));
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(xrandr_output);
                }
            }
        }

        Ok(outputs.into_values().collect())
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Workstation<'out> {
    laptop: Option<&'out Output>,
    externals: Option<(&'out Output, Vec<&'out Output>)>,
    disconnected_externals: Vec<&'out Output>,
}

impl<'out> TryFrom<&'out [Output]> for Workstation<'out> {
    type Error = Error;

    fn try_from(value: &'out [Output]) -> Result<Self, Self::Error> {
        let (mut laptops, mut non_laptops): (Vec<_>, Vec<_>) = value
            .iter()
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

        let (connected_externals, disconnected_externals): (Vec<_>, Vec<_>) = non_laptops
            .into_iter()
            .partition(|output| output.connection_state == OutputConnectionState::Connected);

        let (mut externals, rest): (Vec<_>, Vec<_>) = connected_externals
            .into_iter()
            .partition(|output| output.class == OutputClass::External);

        if laptop.is_none() && externals.is_empty() {
            return Err(Error::Workstation("no screens found".into()));
        }

        let externals = match externals.len() {
            0 => None,
            _ => Some((externals.remove(0), externals)),
        };

        if !rest.is_empty() {
            return Err(Error::Generic(
                "screens that are neither External nor Laptop found".into(),
            ));
        }

        Ok(Self {
            laptop,
            externals,
            disconnected_externals,
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum OutputActiveState {
    On,
    Off,
}

impl fmt::Display for OutputActiveState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::On => "on",
                Self::Off => "off",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum OutputState {
    Connected(OutputActiveState),
    Disconnected,
}

impl fmt::Display for OutputState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Connected(ref active_state) => format!("connected({})", active_state),
                Self::Disconnected => "disconnected".to_owned(),
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
struct OutputSetting<'out> {
    output: &'out Output,
    state: OutputState,
}

impl fmt::Display for OutputSetting<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "output {} to {}", self.output, self.state)
    }
}

#[derive(Debug)]
struct Workspaces<'out>(Vec<Workspace<'out>>);

impl<'out> Workspaces<'out> {
    fn convert(workspaces: i3::Workspaces, outputs: &[&'out Output]) -> Result<Self, Error> {
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
                                Error::Generic(
                                    format!(
                                        "output of workspace {} ({}) not found in i3 outputs",
                                        from.num, from.output
                                    )
                                    .into(),
                                )
                            })?,
                    })
                })
                .collect::<Result<Vec<Workspace>, Error>>()?,
        ))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Workspace<'out> {
    num: usize,
    name: String,
    output: &'out Output,
}

impl fmt::Display for Workspace<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {} on {}", self.num, self.name, self.output)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct WorkspaceSetting<'ws, 'out> {
    workspace: &'ws Workspace<'out>,
    output: &'out Output,
}

impl fmt::Display for WorkspaceSetting<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "workspace {} to output {}", self.workspace, self.output)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Plan<'ws, 'out> {
    output_settings: Vec<OutputSetting<'out>>,
    workspace_settings: Vec<WorkspaceSetting<'ws, 'out>>,
}

impl fmt::Display for Plan<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for output in &self.output_settings {
            write!(f, "{output}")?;
        }
        for workspace in &self.workspace_settings {
            write!(f, "{workspace}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
enum Command<'out, 'args> {
    Xrandr(String, Vec<&'args str>),
    MoveWorkspace { num: usize, output: &'out Output },
}

impl fmt::Display for Command<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Xrandr(ref program, ref args) => write!(f, "{} {}", program, args.join(" ")),
            Self::MoveWorkspace { num, output } => {
                write!(f, "move workspace {num} to {}", output.name)
            }
        }
    }
}

impl<'ws, 'out> Plan<'ws, 'out> {
    fn diagram(&self, f: &mut impl fmt::Write) -> fmt::Result {
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

    fn commands(&self) -> Vec<Command<'out, 'out>> {
        let mut commands = vec![];
        let mut args = vec![];

        let mut left: Option<&OutputSetting> = None;

        for setting in &self.output_settings {
            args.push("--output");
            args.push(&setting.output.name);

            match setting.state {
                OutputState::Connected(OutputActiveState::On) => {
                    args.push("--auto");
                    if let Some(left) = left {
                        args.push("--right-of");
                        args.push(&left.output.name);
                    }
                    left = Some(setting);
                }
                OutputState::Connected(OutputActiveState::Off) | OutputState::Disconnected => {
                    args.push("--off")
                }
            };
        }

        commands.push(Command::Xrandr("xrandr".into(), args));

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

    fn apply(self, i3: &mut i3::Connection) -> Result<Vec<Command<'out, 'out>>, Error> {
        let commands = self.commands();
        for command in &commands {
            match *command {
                Command::Xrandr(ref program, ref args) => {
                    let output = process::Command::new(program)
                        .args(args)
                        .output()
                        .map_err(|e| Error::Command(e.to_string().into()))?;

                    output
                        .status
                        .success()
                        .then_some(())
                        .ok_or(Error::Apply(String::from_utf8(output.stderr)?.into()))?;
                }
                Command::MoveWorkspace { num, output } => {
                    i3.command(i3::Command::MoveWorkspace {
                        id: num,
                        output: output.name.clone(),
                    })?;
                }
            }
        }

        // apply the workspace moves again. this may be necessary because i3 auto-assigns a new workspace
        // when activating a new output. this new workspace may actually belong to a different output.
        for command in &commands {
            if let Command::MoveWorkspace { num, output } = command {
                i3.command(i3::Command::MoveWorkspace {
                    id: *num,
                    output: output.name.clone(),
                })?;
            }
        }

        Ok(commands)
    }
}

impl<'ws, 'out> Workstation<'out> {
    fn all_on_laptop(
        workspaces: &'ws Workspaces<'out>,
        laptop: &'out Output,
        externals: Option<(&'out Output, Vec<&'out Output>)>,
        disconnected_externals: Vec<&'out Output>,
    ) -> Plan<'ws, 'out> {
        Plan {
            output_settings: {
                let mut outputs = vec![laptop.on()];
                outputs.append({
                    &mut match externals {
                        None => vec![],
                        Some((ext, rest)) => {
                            let mut v = vec![ext.off()];
                            v.append(&mut rest.iter().map(|ext| ext.off()).collect());
                            v
                        }
                    }
                });
                outputs.extend(
                    disconnected_externals
                        .into_iter()
                        .map(|output| output.off()),
                );
                outputs
            },
            workspace_settings: workspaces
                .0
                .iter()
                .filter(|workspace| (workspace.output != laptop))
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
        externals: &(&'out Output, Vec<&'out Output>),
        disconnected_externals: Vec<&'out Output>,
    ) -> Result<Plan<'ws, 'out>, Error> {
        Ok(Plan {
            output_settings: {
                let mut v = {
                    let mut v = vec![externals.0.on()];
                    v.append(&mut externals.1.iter().map(|output| output.on()).collect());
                    v
                };
                if let Some(laptop) = laptop {
                    v.push(laptop.off());
                }
                v.extend(
                    disconnected_externals
                        .into_iter()
                        .map(|output| output.off()),
                );
                v
            },
            workspace_settings: {
                let mut v = vec![];
                for workspace in &workspaces.0 {
                    let target_output = match workspace.num {
                        1..=5 => externals.0,
                        6..=10 => match externals.1.len() {
                            0 => externals.0,
                            1 => externals.1.first().expect("checked for len() above"),
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
                            workspace,
                            output: target_output,
                        });
                    }
                }
                v
            },
        })
    }

    fn distribute_workspaces(
        workspaces: &'ws Workspaces<'out>,
        laptop: &'out Output,
        externals: &(&'out Output, Vec<&'out Output>),
    ) -> Result<Vec<WorkspaceSetting<'ws, 'out>>, Error> {
        let mut v = vec![];
        for workspace in &workspaces.0 {
            let target_output = match workspace.num {
                7..=10 => laptop,
                i @ 1..=6 => match externals.1.len() {
                    0 => externals.0,
                    1 => match i {
                        1..=3 => externals.0,
                        4..=6 => externals.1.first().expect("checked for len() above"),
                        _ => unreachable!("checked the range above"),
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
                    workspace,
                    output: target_output,
                });
            }
        }
        Ok(v)
    }

    fn plan(
        &self,
        setup: Setup,
        workspaces: &'ws Workspaces<'out>,
    ) -> Result<Plan<'ws, 'out>, Error> {
        match setup {
            setup @ (Setup::LaptopLeft | Setup::LaptopRight) => match self.laptop {
                None => Err(Error::Plan("no laptop screen found".into())),
                Some(laptop) => {
                    let Some(ref externals) = self.externals else {
                        return Err(Error::Plan("no external screens found".into()));
                    };
                    let workspace_settings =
                        Self::distribute_workspaces(workspaces, laptop, externals)?;

                    let mut output_settings = vec![externals.0.on()];
                    output_settings.append(&mut externals.1.iter().map(|ext| ext.on()).collect());
                    output_settings.extend(
                        self.disconnected_externals
                            .iter()
                            .map(|output| output.off()),
                    );

                    match setup {
                        Setup::LaptopLeft => output_settings.insert(0, laptop.on()),
                        Setup::LaptopRight => output_settings.push(laptop.on()),
                        Setup::LaptopOnly | Setup::ExternalOnly => {
                            unreachable!("checked for enum values above")
                        }
                    }

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
                    self.externals.clone(),
                    self.disconnected_externals.clone(),
                )),
            },
            Setup::ExternalOnly => match self.externals {
                None => Err(Error::Plan("no external screens found".into())),
                Some(ref externals) => Ok(Self::all_on_external(
                    workspaces,
                    self.laptop,
                    externals,
                    self.disconnected_externals.clone(),
                )?),
            },
        }
    }
}

impl TryFrom<i3::Output> for Output {
    type Error = Error;

    fn try_from(value: i3::Output) -> Result<Self, Self::Error> {
        let class = OutputClass::try_detect(&value.name)?;

        Ok(Self {
            name: value.name,
            class,
            // all outputs detected by i3 are implicitly connected
            connection_state: OutputConnectionState::Connected,
        })
    }
}

impl TryFrom<xrandr::Output> for Output {
    type Error = Error;

    fn try_from(value: xrandr::Output) -> Result<Self, Self::Error> {
        let class = OutputClass::try_detect(&value.name)?;

        Ok(Self {
            name: value.name,
            class,
            connection_state: value.state.into(),
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

    #[arg(long)]
    config: Option<String>,
}

const XDG_CONFIG_HOME: &str = "XDG_CONFIG_HOME";

#[expect(clippy::print_stdout, reason = "main")]
fn run() -> Result<(), Error> {
    let args = Cli::parse();

    let config = match args.config {
        Some(path) => {
            let path = PathBuf::from(path);
            match config::from_path(&path)? {
                Some(c) => Ok(Some(c)),
                None => Err(Error::ConfigNotFound { path }),
            }
        }
        None => {
            let mut config_home = match env::var(XDG_CONFIG_HOME) {
                Ok(v) => Ok(PathBuf::from(v)),
                Err(e) => match e {
                    env::VarError::NotPresent => match env::var("HOME") {
                        Ok(v) => Ok([&v, ".config"].iter().collect::<PathBuf>()),
                        Err(e) => match e {
                            env::VarError::NotPresent => Err(Error::Generic("HOME not set".into())),
                            env::VarError::NotUnicode(_) => {
                                Err(Error::Generic("HOME contains invalid unicode".into()))
                            }
                        },
                    },
                    env::VarError::NotUnicode(_) => Err(Error::Generic(
                        "{XDG_CONFIG_HOME} env variable is not unicode".into(),
                    )),
                },
            }?;
            config_home.push("screencfg.toml");
            Ok(config::from_path(&config_home)?)
        }
    }?;

    let mut i3_connection = i3::connect()?;

    let outputs = Output::findall(&mut i3_connection)?;

    if args.debug {
        println!("i3 outputs:");
        for output in &outputs {
            println!("  - {output}");
        }
        println!();
    }

    let workstation: Workstation = (&*outputs).try_into()?;

    let workspaces = i3_connection.workspaces()?;
    let workspaces = Workspaces::convert(workspaces, &outputs.iter().collect::<Vec<&Output>>())?;

    if args.debug {
        println!("i3 workspaces:");
        for workspace in &workspaces.0 {
            println!("  - {workspace}");
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
            .map_err(|_| Error::Plan("no plan fit with \"best\" strategy".into()))?
    };

    if args.debug {
        println!("{plan}");
        println!();
    }
    if args.diagram {
        let mut buf = String::new();
        plan.diagram(&mut buf)?;

        println!("{buf}");
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

    if let Some(post_commands) = config.and_then(|c| c.post_commands) {
        for command in post_commands {
            println!("executing post command \"{command}\"");
            let output = process::Command::new("bash")
                .arg("-c")
                .arg(&command)
                .output()
                .map_err(|e| {
                    Error::Generic(
                        format!("post command \"{command}\" invocation failed: {e}").into(),
                    )
                })?;

            if !output.status.success() {
                return Err(Error::Generic(
                    format!(
                        "post command \"{command}\" failed: {stderr}",
                        stderr = String::from_utf8(output.stderr)
                            .unwrap_or_else(|_| "stderr invalid utf8".to_owned())
                    )
                    .into(),
                ));
            }
        }
    }

    Ok(())
}

#[allow(clippy::print_stderr, reason = "main")]
fn main() -> process::ExitCode {
    process::ExitCode::from(match run() {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{e}");
            1
        }
    })
}

#[cfg(test)]
mod tests {
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
}
