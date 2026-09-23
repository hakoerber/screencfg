use std::{env, fmt, num::ParseIntError, path::PathBuf, process, time};

use i3::Conn as _;

mod error;
use error::Error;

mod cli;
mod config;
mod udev;

#[cfg(test)]
mod tests;

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
            .filter(|output| output.active)
            .map(TryInto::try_into)
            .collect::<Result<Vec<Self>, Error>>()?;

        let xrandr_outputs = xrandr::Output::findall()?
            .into_iter()
            .map(TryInto::try_into)
            .collect::<Result<Vec<Self>, Error>>()?;

        let mut outputs = i3_outputs;

        for xrandr_output in xrandr_outputs {
            match outputs
                .iter_mut()
                .find(|output| output.name == xrandr_output.name)
            {
                Some(existing) => {
                    if existing.connection_state != xrandr_output.connection_state {
                        // if there is a connection state mismatch, we go with i3, as xrandr may still
                        // have inactive outputs maked as active
                    }
                }
                // if i3 does not know about the output, we use the xrandr state as-is
                None => {
                    outputs.push(xrandr_output);
                }
            }
        }

        Ok(outputs)
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
                ));
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
            writeln!(f, "{output}")?;
        }
        for workspace in &self.workspace_settings {
            writeln!(f, "{workspace}")?;
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
                outputs.append(&mut match externals {
                    None => vec![],
                    Some((ext, rest)) => {
                        let mut v = vec![ext.off()];
                        v.append(&mut rest.iter().map(|ext| ext.off()).collect());
                        v
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
        externals: &(&'out Output, Vec<&'out Output>),
        disconnected_externals: Vec<&'out Output>,
        external_ordering: &ExternalOrdering,
    ) -> Result<Plan<'ws, 'out>, Error> {
        let externals: Vec<&Output> = {
            let mut v = vec![externals.0];
            v.extend(&externals.1);
            v
        };

        // shuffle around if required
        let externals = match external_ordering {
            ExternalOrdering::Default => externals,
            ExternalOrdering::Custom { order } => {
                let mut out = Vec::with_capacity(externals.len());
                assert_eq!(
                    order.len(),
                    externals.len(),
                    "assured during creation of order"
                );

                for order in order {
                    out.push(
                        *externals
                            .get(order - 1)
                            .expect("order contains incrementing integers"),
                    )
                }

                out
            }
        };

        Ok(Plan {
            output_settings: {
                let mut outputs: Vec<_> = externals.iter().map(|output| output.on()).collect();

                if let Some(laptop) = laptop {
                    outputs.push(laptop.off());
                }
                outputs.extend(
                    disconnected_externals
                        .into_iter()
                        .map(|output| output.off()),
                );
                outputs
            },
            workspace_settings: {
                let mut v = vec![];
                for workspace in &workspaces.0 {
                    let target_output = match workspace.num {
                        1..=5 => externals[0],
                        6..=10 => match externals.len() {
                            1 => externals[0],
                            2 => externals[1],
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
            let target_output = match workspace.num {
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
        externals: &[&'out Output],
    ) -> Result<Vec<WorkspaceSetting<'ws, 'out>>, Error> {
        let mut v = vec![];
        for workspace in &workspaces.0 {
            let target_output = match workspace.num {
                7..=10 => laptop,
                i @ 1..=6 => match externals.len() {
                    1 => externals[0],
                    2 => match i {
                        1 => externals[0],
                        2..=6 => externals[1],
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

    fn plan(
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
                    let externals: Vec<&Output> = {
                        let mut v = vec![externals.0];
                        v.extend(&externals.1);
                        v
                    };

                    // shuffle around if required
                    let externals = match external_ordering {
                        ExternalOrdering::Default => externals,
                        ExternalOrdering::Custom { order } => {
                            let mut out = Vec::with_capacity(externals.len());
                            assert_eq!(
                                order.len(),
                                externals.len(),
                                "assured during creation of order"
                            );

                            for order in order {
                                out.push(
                                    *externals
                                        .get(order - 1)
                                        .expect("order contains incrementing integers"),
                                )
                            }

                            out
                        }
                    };

                    let workspace_settings =
                        Self::distribute_workspaces(workspaces, laptop, &externals)?;

                    let mut output_settings: Vec<OutputSetting> =
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

                    if !externals.1.is_empty() {
                        return Err(Error::Plan(
                            "can only project with single external screen".into(),
                        ));
                    }

                    let external = externals.0;

                    let workspace_settings = Self::projector(workspaces, laptop, external)?;

                    let mut output_settings = vec![laptop.on(), externals.0.on()];
                    output_settings.append(&mut externals.1.iter().map(|ext| ext.on()).collect());
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
                    external_ordering,
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

#[derive(Debug, Clone, Copy)]
enum Setup {
    LaptopLeft,
    LaptopRight,
    LaptopOnly,
    ExternalOnly,
    Projector,
}

impl From<cli::Setup> for Setup {
    fn from(value: cli::Setup) -> Self {
        match value {
            cli::Setup::LaptopLeft => Self::LaptopLeft,
            cli::Setup::LaptopRight => Self::LaptopRight,
            cli::Setup::LaptopOnly => Self::LaptopOnly,
            cli::Setup::ExternalOnly => Self::ExternalOnly,
            cli::Setup::Projector => Self::Projector,
        }
    }
}

#[derive(Debug)]
enum ExternalOrdering {
    Default,
    Custom { order: Vec<usize> },
}

const XDG_CONFIG_HOME: &str = "XDG_CONFIG_HOME";

fn find_config(path: Option<String>) -> Result<Option<config::Config>, Error> {
    match path {
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
    }
}

fn manage_screens(
    config: Option<&config::Config>,
    debug: bool,
    dry_run: bool,
    diagram: bool,
    approach: cli::Approach,
    custom_external_ordering: Option<&str>,
) -> Result<(), Error> {
    let mut i3_connection = i3::connect()?;

    let outputs = Output::findall(&mut i3_connection)?;
    let workstation: Workstation = (&*outputs).try_into()?;

    let workspaces = i3_connection.workspaces()?;
    let workspaces = Workspaces::convert(workspaces, &outputs.iter().collect::<Vec<&Output>>())?;

    if debug {
        println!("=== workstation setup:");

        println!("=== laptop output:");
        match workstation.laptop {
            Some(output) => {
                println!(
                    "{name} ({state})",
                    name = output.name,
                    state = output.connection_state
                );
            }
            None => println!("none"),
        }

        println!("=== external outputs:");
        match workstation.externals {
            Some((first, ref rest)) => {
                let mut outputs = vec![first];
                outputs.extend(rest);
                for output in outputs {
                    println!(
                        "{name} ({state})",
                        name = output.name,
                        state = output.connection_state
                    );
                }
            }
            None => println!("none"),
        }
        println!("=== i3 workspaces:");
        for workspace in &workspaces.0 {
            println!("  - {workspace}");
        }
        println!();
    }

    i3_connection.command(i3::Command::Nop)?;

    let external_ordering = match custom_external_ordering {
        Some(order) => {
            let elems = order
                .split(',')
                .map(|elem| elem.parse::<usize>())
                .collect::<Result<Vec<usize>, ParseIntError>>()
                .map_err(|err| {
                    Error::Command(format!("could not parse order as integer: {err}").into())
                })?;

            if match workstation.externals {
                Some(ref externals) => externals.1.len() != elems.len() - 1,
                None => !elems.is_empty(),
            } {
                return Err(Error::Command(
                    "custom ordering needs to be the same length as number of outputs".into(),
                ));
            }

            let sorted = {
                let mut elems = elems.clone();
                elems.sort();
                elems
            };

            if sorted != (1..=(elems.len())).collect::<Vec<usize>>() {
                return Err(Error::Command(
                    "custom ordering needs to contain incrementing integers only".into(),
                ));
            }

            ExternalOrdering::Custom { order: elems }
        }
        None => ExternalOrdering::Default,
    };

    let plan = if let Some(setup) = approach.setup {
        workstation.plan(setup.into(), &workspaces, &external_ordering)?
    } else {
        workstation
            .plan(Setup::LaptopLeft, &workspaces, &external_ordering)
            .or_else(|_| workstation.plan(Setup::LaptopOnly, &workspaces, &external_ordering))
            .or_else(|_| workstation.plan(Setup::ExternalOnly, &workspaces, &external_ordering))
            .map_err(|_| Error::Plan("no plan fit with \"best\" strategy".into()))?
    };

    if debug {
        println!("{plan}");
    }

    if diagram {
        let mut buf = String::new();
        plan.diagram(&mut buf)?;

        println!("{buf}\n");
    }

    let commands = if dry_run {
        plan.commands()
    } else {
        plan.apply(&mut i3_connection)?
    };

    println!("applying changes:");
    for command in commands {
        println!("- {command}");
    }

    if let Some(post_commands) = config.and_then(|c| c.post_commands.as_ref()) {
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

#[expect(clippy::print_stdout, reason = "main")]
fn run() -> Result<(), Error> {
    let args = cli::Cli::parse();

    match args.subcommand {
        cli::Cmd::Set(set_options) => {
            let config = find_config(args.config)?;

            manage_screens(
                config.as_ref(),
                args.debug,
                set_options.dry_run,
                set_options.diagram,
                set_options.approach,
                set_options.custom_external_ordering.as_deref(),
            )?;
        }
        #[allow(unused)]
        cli::Cmd::Watch(watch_options) => {
            let config = find_config(args.config)?;

            if watch_options.once {
                manage_screens(
                    config.as_ref(),
                    args.debug,
                    watch_options.dry_run,
                    watch_options.diagram,
                    watch_options.approach,
                    watch_options.custom_external_ordering.as_deref(),
                )?;
            }

            // used to differentiate between multiple event streams / sockets. We only have one, so
            // we can use any constant value.
            const TOKEN: mio::Token = mio::Token(0);

            let mut events = mio::Events::with_capacity(1024);

            let socket = udev::EventListener::new(udev::Subsystem::Drm)?;

            let mut stream = udev::EventStream::from_listener(socket, TOKEN)?;

            let err = stream.handle(
                |event_type| {
                    matches!(
                        event_type,
                        udev::EventType::Add | udev::EventType::Remove | udev::EventType::Change
                    )
                },
                move |event| {
                    if args.debug {
                        println!("Received event: {event:?}");
                    }
                    manage_screens(
                        config.as_ref(),
                        args.debug,
                        watch_options.dry_run,
                        watch_options.diagram,
                        watch_options.approach,
                        watch_options.custom_external_ordering.as_deref(),
                    )
                },
                time::Duration::from_secs(1),
            );

            eprintln!("{err}");
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
