use std::{borrow::Cow, fmt};

use serde::Deserialize;

use super::{
    Connection, Error, EventType, Height, Output, OutputMode, OutputName, OutputNameError,
    OutputNameRef, Outputs, RefreshRate, Version, Width, Workspace, WorkspaceName, WorkspaceNumber,
    Workspaces,
};

// Sic, sway resuses the i3 magic string for compatibility
const MAGIC_STRING: &str = "i3-ipc";

#[derive(Debug, Clone, Copy, derive_more::Display)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub struct PosX(pub usize);

#[derive(Debug, Clone, Copy, derive_more::Display)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub struct PosY(pub usize);

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub struct Position {
    pub x: PosX,
    pub y: PosY,
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub enum OutputState {
    Enabled {
        position: Position,
        refresh_rate: RefreshRate,
    },
    Disabled,
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub enum Command<'out> {
    Nop,
    Output {
        output_name: OutputNameRef<'out>,
        state: OutputState,
    },
    MoveWorkspace {
        number: WorkspaceNumber,
        output_name: OutputNameRef<'out>,
    },
    Reload,
}

#[derive(Debug)]
pub(crate) enum Message<'input> {
    Command(Command<'input>),
    Subscribe(&'input [EventType]),
    Workspaces,
    Outputs,
    Version,
}

impl From<Message<'_>> for u32 {
    fn from(value: Message<'_>) -> Self {
        match value {
            Message::Command(_) => 0,
            Message::Subscribe(_) => 2,
            Message::Workspaces => 1,
            Message::Outputs => 3,
            Message::Version => 7,
        }
    }
}

impl Message<'_> {
    fn bytes(self) -> Result<Vec<u8>, Error> {
        let payload: Option<Cow<'static, str>> = match self {
            Self::Command(ref command) => Some(command.into()),
            Self::Subscribe(ref event_types) => Some(
                serde_json::to_string(event_types)
                    .expect("serializing static values always succeeds")
                    .into(),
            ),
            Self::Workspaces | Self::Outputs | Self::Version => None,
        };

        let mut message: Vec<u8> = vec![];
        let command_number: u32 = self.into();

        message.extend_from_slice(MAGIC_STRING.as_bytes());
        message.extend_from_slice(
            &u32::try_from(payload.as_ref().map_or(0, |l| l.len()))
                .map_err(|_err| Error::Protocol("payload length bigger than 4 bytes".into()))?
                .to_ne_bytes(),
        );
        message.extend_from_slice(&(command_number.to_ne_bytes()));
        if let Some(payload) = payload {
            message.extend_from_slice(payload.as_bytes());
        }
        Ok(message)
    }

    pub(crate) async fn send(self, socket: &mut Connection) -> Result<(), Error> {
        tracing::debug!(message = ?self, "sending sway message");
        let message = self.bytes()?;
        tracing::debug!("sending bytes {message:?}");
        socket.write_all(&message).await?;
        tracing::debug!("bytes sent");
        Ok(())
    }
}

impl From<&Command<'_>> for Cow<'static, str> {
    fn from(value: &Command<'_>) -> Self {
        match *value {
            Command::Nop => Cow::from("nop"),
            Command::MoveWorkspace {
                number: id,
                ref output_name,
            } => Cow::from(format!(
                "[workspace=\"{id}\"] move workspace to output {output_name}"
            )),
            Command::Reload => Cow::from("reload"),
            Command::Output {
                ref output_name,
                ref state,
            } => match *state {
                OutputState::Enabled {
                    ref position,
                    refresh_rate: ref _refresh_rate,
                } => Cow::from(format!(
                    "output \"{output_name}\" position {x} {y}",
                    x = position.x,
                    y = position.y,
                )),
                OutputState::Disabled => Cow::from(format!("output \"{output_name}\" disable")),
            },
        }
    }
}

#[derive(Debug, serde::Deserialize)]
pub(crate) struct OutputRectPayload {
    x: usize,
    y: usize,
}

#[derive(Debug, serde::Deserialize)]
pub(crate) struct OutputPayload {
    name: String,
    active: bool,
    current_mode: OutputModePayload,
    rect: OutputRectPayload,
}

#[derive(Debug, serde::Deserialize)]
pub(crate) struct OutputModePayload {
    width: usize,
    height: usize,
    refresh: usize,
}

impl TryFrom<OutputPayload> for Output {
    type Error = OutputNameError;

    fn try_from(value: OutputPayload) -> Result<Self, Self::Error> {
        Ok(Self {
            name: OutputName::try_new(value.name)?,
            active: value.active,
            mode: OutputMode {
                width: Width::new(value.current_mode.width),
                height: Height::new(value.current_mode.height),
                refresh_rate: RefreshRate::Ms(value.current_mode.refresh),
            },
            position: Position {
                x: PosX(value.rect.x),
                y: PosY(value.rect.y),
            },
        })
    }
}

impl TryFrom<Vec<OutputPayload>> for Outputs {
    type Error = OutputNameError;

    fn try_from(value: Vec<OutputPayload>) -> Result<Self, Self::Error> {
        Ok(Self(
            value
                .into_iter()
                .filter(|output| output.name != "xroot-0")
                .map(OutputPayload::try_into)
                .collect::<Result<_, OutputNameError>>()?,
        ))
    }
}

#[derive(Debug, serde::Deserialize)]
#[expect(dead_code, reason = "monitor data defintion")]
pub(crate) struct WorkspacePayload {
    id: usize,
    num: usize,
    name: String,
    output: String,
}

impl TryFrom<WorkspacePayload> for Workspace {
    type Error = Error;

    fn try_from(value: WorkspacePayload) -> Result<Self, Self::Error> {
        Ok(Self {
            num: WorkspaceNumber::new(value.num),
            name: WorkspaceName::try_new(value.name)?,
            output: OutputName::try_new(value.output)?,
        })
    }
}

impl TryFrom<Vec<WorkspacePayload>> for Workspaces {
    type Error = Error;

    fn try_from(value: Vec<WorkspacePayload>) -> Result<Self, Self::Error> {
        Ok(Self(
            value
                .into_iter()
                .map(TryInto::try_into)
                .collect::<Result<_, Error>>()?,
        ))
    }
}

#[derive(Debug, serde::Deserialize)]
#[expect(dead_code, reason = "monitor data defintion")]
pub(crate) struct VersionPayload {
    human_readable: String,
    loaded_config_file_name: String,
    major: usize,
    minor: usize,
    patch: usize,
}

impl From<VersionPayload> for Version {
    fn from(value: VersionPayload) -> Self {
        Self {
            major: value.major,
            minor: value.minor,
            patch: value.patch,
        }
    }
}

#[derive(Debug, serde::Deserialize)]
pub(crate) struct CommandPayload {
    pub(crate) success: bool,
    pub(crate) error: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct EventPayloadWorkspace {
    pub change: String,
}

impl fmt::Display for EventPayloadWorkspace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "change: {}", self.change)
    }
}

#[derive(Debug, serde::Deserialize)]
pub(crate) struct SubscriptionPayload {
    pub(crate) success: bool,
    pub(crate) error: Option<String>,
}

#[derive(Debug, strum::EnumDiscriminants, Clone)]
#[strum_discriminants(derive(Hash), name(EventPayloadType))]
pub enum EventPayload {
    Output,
    Workspace(EventPayloadWorkspace),
}

impl fmt::Display for EventPayload {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Output => write!(f, "Output"),
            Self::Workspace(ref event_payload_workspace) => {
                write!(f, "Workspace({event_payload_workspace})")
            }
        }
    }
}

#[derive(Debug, strum::EnumDiscriminants)]
#[strum_discriminants(derive(strum::Display), vis(pub), name(ResponseType))]
pub(crate) enum Response {
    Version(VersionPayload),
    Workspaces(Vec<WorkspacePayload>),
    Command(Vec<CommandPayload>),
    Outputs(Vec<OutputPayload>),
    Subscription(SubscriptionPayload),
    SubscriptionEvent(EventPayload),
}

impl Response {
    pub(crate) async fn read(stream: &mut Connection) -> Result<Self, Error> {
        tracing::debug!("reading response");
        let mut response =
            vec![
                0;
                MAGIC_STRING
                    .chars()
                    .count()
                    .checked_add(
                        // for response length
                        4 +
                    // for command
                    4
                    )
                    .ok_or_else(|| { Error::Protocol("payload length overflowed".into()) })?
            ];

        let _bytes_count = stream.read_exact(&mut response).await?;

        tracing::debug!("received response {response:?}");

        if response
            .get(0..6)
            .ok_or_else(|| Error::Protocol("response too short for even the magic string".into()))?
            != MAGIC_STRING.as_bytes()
        {
            return Err(Error::Protocol("magic string not found".into()));
        }
        let response_length = {
            let bytes = response
                .get(6..10)
                .ok_or_else(|| Error::Protocol("not enough bytes for response length".into()))?;

            let bytes = bytes
                .try_into()
                .expect("slice of length 4 can always be converted into an array of size 4");

            u32::from_ne_bytes(bytes)
        };

        let response_command = {
            let bytes = response
                .get(10..14)
                .ok_or_else(|| Error::Protocol("not enough bytes for command".into()))?;

            let bytes = bytes
                .try_into()
                .expect("slice of length 4 can always be converted into an array of size 4");

            u32::from_ne_bytes(bytes)
        };

        response = vec![
            0;
            response_length
                .try_into()
                .map_err(|_err| { Error::Protocol("u32 overflowed usize".into()) })?
        ];

        let _bytes_count = stream.read_exact(&mut response).await?;

        // highest bit indicates event response
        if response_command >> 31_u8 == 1 {
            let response_command = response_command & !(1 << 31_u8);
            Ok(Self::SubscriptionEvent(match response_command {
                0 => EventPayload::Workspace(serde_json::from_slice(&response)?),
                1 => {
                    #[derive(Debug, serde::Deserialize)]
                    struct EventResponseOutput {
                        change: String,
                    }

                    let response: EventResponseOutput = serde_json::from_slice(&response)?;
                    assert_eq!(response.change, "unspecified", "this is a static response");
                    EventPayload::Output
                }
                id => return Err(Error::UnknownResponseCommand { id, event: true }),
            }))
        } else {
            match response_command {
                0 => Ok(Self::Command(serde_json::from_slice(&response)?)),
                1 => Ok(Self::Workspaces(serde_json::from_slice(&response)?)),
                2 => Ok(Self::Subscription(serde_json::from_slice(&response)?)),
                3 => Ok(Self::Outputs(serde_json::from_slice(&response)?)),
                7 => Ok(Self::Version(serde_json::from_slice(&response)?)),
                id => Err(Error::UnknownResponseCommand { id, event: false }),
            }
        }
    }
}
