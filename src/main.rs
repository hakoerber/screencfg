use std::{
    env,
    ffi::OsStr,
    fmt,
    num::ParseIntError,
    path::{Path, PathBuf},
    process, time,
};

use i3::Conn as _;

mod cli;
mod config;
mod error;
mod nonempty;
mod udev;

use error::Error;
use nonempty::NonEmptyVec;

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
    fn try_detect(value: &OutputName) -> Result<Self, Error> {
        if value.as_str().starts_with("eDP-") {
            Ok(Self::Laptop)
        } else if value.as_str().starts_with("DP-")
            || value.as_str().starts_with("HDMI-")
            || value.as_str().starts_with("DisplayPort-")
        {
            Ok(Self::External)
        } else {
            Err(Error::Classify(
                format!("could not classify output: {value}").into(),
            ))
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct OutputName(String);

impl OutputName {
    fn as_str(&self) -> &str {
        &self.0
    }

    fn into_string(self) -> String {
        self.0
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

impl From<xrandr::OutputName> for OutputName {
    fn from(value: xrandr::OutputName) -> Self {
        Self(value.into_string())
    }
}

impl From<i3::OutputName> for OutputName {
    fn from(value: i3::OutputName) -> Self {
        Self(value.into_string())
    }
}

impl From<OutputName> for i3::OutputName {
    fn from(value: OutputName) -> Self {
        Self::new(value.into_string())
    }
}

impl fmt::Display for OutputName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
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
            match *self {
                Self::Connected => "connected",
                Self::Disconnected => "disconnected",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Output {
    class: OutputClass,
    name: OutputName,
    connection_state: OutputConnectionState,
}

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.class)
    }
}

impl<'out> Output {
    fn on(&'out self) -> OutputSetting<'out> {
        assert!(
            self.connection_state == OutputConnectionState::Connected,
            "tried to activate disconnected output"
        );
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
    externals: Option<NonEmptyVec<&'out Output>>,
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

        let (externals, rest): (Vec<_>, Vec<_>) = connected_externals
            .into_iter()
            .partition(|output| output.class == OutputClass::External);

        if laptop.is_none() && externals.is_empty() {
            return Err(Error::Workstation("no screens found".into()));
        }

        let externals = match externals.len() {
            0 => None,
            _ => Some(NonEmptyVec::new(externals)),
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
            match *self {
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
                Self::Connected(ref active_state) => format!("connected({active_state})"),
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
                    let output_name_from_i3_workspaces: OutputName = from.output.into();
                    Ok(Workspace {
                        num: from.num.into(),
                        name: from.name.into(),
                        output: outputs
                            .iter()
                            .find(|output| output_name_from_i3_workspaces == output.name)
                            .ok_or_else(|| {
                                Error::Generic(
                                    format!(
                                        "output of workspace {} ({}) not found in i3 outputs",
                                        from.num, output_name_from_i3_workspaces
                                    )
                                    .into(),
                                )
                            })?,
                    })
                })
                .collect::<Result<Vec<Workspace<'_>>, Error>>()?,
        ))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct WorkspaceName(String);

impl fmt::Display for WorkspaceName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<i3::WorkspaceName> for WorkspaceName {
    fn from(value: i3::WorkspaceName) -> Self {
        Self(value.into_inner())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
struct WorkspaceNumber(usize);

impl fmt::Display for WorkspaceNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<WorkspaceNumber> for i3::WorkspaceNumber {
    fn from(value: WorkspaceNumber) -> Self {
        Self::new(value.0)
    }
}

impl From<i3::WorkspaceNumber> for WorkspaceNumber {
    fn from(value: i3::WorkspaceNumber) -> Self {
        Self(value.into_inner())
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Workspace<'out> {
    num: WorkspaceNumber,
    name: WorkspaceName,
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
struct Program(&'static str);

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
struct Arg<'a>(&'a str);

impl AsRef<OsStr> for Arg<'_> {
    fn as_ref(&self) -> &OsStr {
        self.0.as_ref()
    }
}

impl Arg<'_> {
    fn as_str(&self) -> &str {
        self.0
    }
}

#[derive(Debug)]
enum Command<'out, 'args> {
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

    fn apply(i3: &mut i3::Connection, commands: Vec<Command<'out, '_>>) -> Result<(), Error> {
        // we need to do the xrandr commands first, as i3 may create new workspaces automatically on newly
        // created outputs.

        enum Either<A, B> {
            A(A),
            B(B),
        }

        struct XrandrCommand<'args> {
            program: Program,
            args: Vec<Arg<'args>>,
        }

        struct MoveWorkspace<'out> {
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
                output: &command.output.name.clone().into(),
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
        disconnected_externals: Vec<&'out Output>,
    ) -> Plan<'ws, 'out> {
        Plan {
            output_settings: {
                let mut outputs = vec![laptop.on()];
                outputs.append(&mut match externals {
                    None => vec![],
                    Some(externals) => externals.iter().map(|ext| ext.off()).collect(),
                });
                outputs.extend(disconnected_externals.into_iter().map(Output::off));
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
        disconnected_externals: Vec<&'out Output>,
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
                outputs.extend(disconnected_externals.into_iter().map(Output::off));
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
        let name: OutputName = value.name.into();

        let class = OutputClass::try_detect(&name)?;

        Ok(Self {
            name,
            class,
            // all outputs detected by i3 are implicitly connected
            connection_state: OutputConnectionState::Connected,
        })
    }
}

impl TryFrom<xrandr::Output> for Output {
    type Error = Error;

    fn try_from(value: xrandr::Output) -> Result<Self, Self::Error> {
        let name: OutputName = value.name.into();

        let class = OutputClass::try_detect(&name)?;

        Ok(Self {
            name,
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

#[derive(Debug)]
enum ExternalOrdering {
    Default,
    Custom { order: Vec<usize> },
}

impl ExternalOrdering {
    fn parse_from_str(input: &str, external_output_count: usize) -> Result<Self, Error> {
        let elems = input
            .split(',')
            .map(str::parse::<usize>)
            .collect::<Result<Vec<usize>, ParseIntError>>()
            .map_err(|err| {
                Error::Command(format!("could not parse order as integer: {err}").into())
            })?;

        if external_output_count != elems.len() {
            return Err(Error::Command(
                "custom ordering needs to be the same length as number of outputs".into(),
            ));
        }

        let sorted = {
            let mut elems = elems.clone();
            elems.sort_unstable();
            elems
        };

        if sorted != (1..=(elems.len())).collect::<Vec<usize>>() {
            return Err(Error::Command(
                "custom ordering needs to contain incrementing integers only".into(),
            ));
        }

        Ok(Self::Custom { order: elems })
    }
}

const XDG_CONFIG_HOME: &str = "XDG_CONFIG_HOME";

fn find_config(path: Option<&Path>) -> Result<Option<config::Config>, Error> {
    match path {
        Some(path) => match config::from_path(path)? {
            Some(c) => Ok(Some(c)),
            None => Err(Error::ConfigNotFound {
                path: path.to_owned(),
            }),
        },
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

#[expect(clippy::print_stdout, reason = "high level function")]
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
    let workstation: Workstation<'_> = (&*outputs).try_into()?;

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
            Some(ref externals) => {
                for output in externals {
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
        Some(order) => ExternalOrdering::parse_from_str(
            order,
            match workstation.externals {
                Some(ref externals) => externals.len(),
                None => 0,
            },
        )?,
        None => ExternalOrdering::Default,
    };

    let plan = match approach {
        cli::Approach::LaptopLeft => {
            workstation.plan(Setup::LaptopLeft, &workspaces, &external_ordering)?
        }
        cli::Approach::LaptopRight => {
            workstation.plan(Setup::LaptopRight, &workspaces, &external_ordering)?
        }
        cli::Approach::LaptopOnly => {
            workstation.plan(Setup::LaptopOnly, &workspaces, &external_ordering)?
        }
        cli::Approach::ExternalOnly => {
            workstation.plan(Setup::ExternalOnly, &workspaces, &external_ordering)?
        }
        cli::Approach::Projector => {
            workstation.plan(Setup::Projector, &workspaces, &external_ordering)?
        }
        cli::Approach::Best => workstation
            .plan(Setup::LaptopLeft, &workspaces, &external_ordering)
            .or_else(|_| workstation.plan(Setup::LaptopOnly, &workspaces, &external_ordering))
            .or_else(|_| workstation.plan(Setup::ExternalOnly, &workspaces, &external_ordering))
            .map_err(|_err| Error::Plan("no plan fit with \"best\" strategy".into()))?,
    };

    if debug {
        println!("{plan}");
    }

    if diagram {
        let mut buf = String::new();
        plan.diagram(&mut buf)
            .map_err(|err| Error::Generic(err.to_string().into()))?;

        println!("{buf}\n");
    }

    let commands = plan.commands();

    if dry_run {
        println!("dry run mode:");
    } else {
        println!("applying changes:");
    }

    for command in &commands {
        println!("- {command}");
    }

    if !dry_run {
        Plan::apply(&mut i3_connection, commands)?;
    }

    if let Some(post_commands) = config.and_then(|c| c.post_commands.as_ref()) {
        for command in post_commands {
            println!("executing post command \"{command}\"");
            let output = process::Command::new("bash")
                .arg("-c")
                .arg(command)
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
#[expect(clippy::print_stderr, reason = "main")]
fn run() -> Result<(), Error> {
    let args = cli::Cli::parse();

    match args.subcommand {
        cli::Cmd::Set(set_options) => {
            let config = find_config(args.config.map(|path| PathBuf::from(path)).as_deref())?;

            manage_screens(
                config.as_ref(),
                args.debug,
                set_options.dry_run,
                set_options.diagram,
                set_options.setup,
                set_options.custom_external_ordering.as_deref(),
            )?;
        }
        cli::Cmd::Watch(watch_options) => {
            // used to differentiate between multiple event streams / sockets. We only have one, so
            // we can use any constant value.
            const TOKEN: mio::Token = mio::Token(0);

            let config = find_config(args.config.map(|path| PathBuf::from(path)).as_deref())?;

            if watch_options.once {
                manage_screens(
                    config.as_ref(),
                    args.debug,
                    watch_options.dry_run,
                    watch_options.diagram,
                    watch_options.setup,
                    watch_options.custom_external_ordering.as_deref(),
                )?;
            }

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
                        println!("{event}");
                    }
                    manage_screens(
                        config.as_ref(),
                        args.debug,
                        watch_options.dry_run,
                        watch_options.diagram,
                        watch_options.setup,
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

#[expect(clippy::print_stderr, reason = "main")]
fn main() -> process::ExitCode {
    process::ExitCode::from(match run() {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{e}");
            1
        }
    })
}
