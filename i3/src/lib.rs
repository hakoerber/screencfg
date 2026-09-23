use std::{
    borrow::Cow,
    collections::{HashMap, hash_map::Entry},
    convert::Infallible,
    ffi::OsStr,
    fmt,
    io::{Read, Write},
    ops::{Deref, DerefMut, Index},
    os::unix::{ffi::OsStrExt as _, net},
    path::PathBuf,
    process,
    sync::mpsc,
    thread,
    time::{Duration, Instant},
    vec::IntoIter,
};

mod error;
pub use error::Error;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy)]
pub struct WorkspaceNumber(usize);

impl WorkspaceNumber {
    pub fn new(value: usize) -> Self {
        Self(value)
    }

    pub fn into_inner(self) -> usize {
        self.0
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct WorkspaceName(String);

impl WorkspaceName {
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl fmt::Display for WorkspaceNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct OutputName(String);

impl OutputName {
    pub fn new(value: String) -> Self {
        Self(value)
    }

    pub fn into_string(self) -> String {
        self.0
    }
}

impl fmt::Display for OutputName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for OutputName {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub struct OutputNameRef<'a>(&'a String);

impl<'a> OutputNameRef<'a> {
    pub fn new(value: &'a String) -> Self {
        Self(value)
    }
}

impl fmt::Display for OutputNameRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub enum Command<'out> {
    Nop,
    MoveWorkspace {
        number: WorkspaceNumber,
        output_name: OutputNameRef<'out>,
    },
    Reload,
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
        }
    }
}

pub trait Conn {
    fn version(&mut self) -> Result<Version, Error>;
    fn outputs(&mut self) -> Result<Outputs, Error>;
    fn workspaces(&mut self) -> Result<Workspaces, Error>;
    fn command(&mut self, command: Command<'_>) -> Result<(), Error>;
    fn subscribe(
        &mut self,
        sender: mpsc::SyncSender<Result<EventPayload, Error>>,
        debounce_time: Duration,
        event_types: &[EventType],
    ) -> Result<Infallible, Error>;
}

#[cfg(any(test, feature = "testing"))]
pub enum MockSetting {
    LaptopOnly,
    ExternalOnly(usize),
    Mixed,
}

#[cfg(any(test, feature = "testing"))]
pub struct MockConnection {
    pub fail: bool,
    pub setting: MockSetting,
}

#[cfg(any(test, feature = "testing"))]
impl MockConnection {
    fn check_fail(&self) -> Result<(), Error> {
        if self.fail {
            Err(Error::Connection("fail".into()))
        } else {
            Ok(())
        }
    }
}

#[cfg(any(test, feature = "testing"))]
impl Conn for MockConnection {
    fn version(&mut self) -> Result<Version, Error> {
        self.check_fail()?;
        Ok(Version {
            minor: 1,
            patch: 2,
            major: 3,
        })
    }

    fn outputs(&mut self) -> Result<Outputs, Error> {
        self.check_fail()?;
        match self.setting {
            MockSetting::LaptopOnly => Ok(Outputs(vec![Output {
                name: OutputName::new("eDP-1".into()),
                active: true,
                primary: true,
            }])),
            MockSetting::ExternalOnly(num) => match num {
                1 => Ok(Outputs(vec![Output {
                    name: OutputName::new("DP-1".into()),
                    active: true,
                    primary: false,
                }])),
                2 => Ok(Outputs(vec![
                    Output {
                        name: OutputName::new("DP-1".into()),
                        active: true,
                        primary: false,
                    },
                    Output {
                        name: OutputName::new("DP-2".into()),
                        active: false,
                        primary: false,
                    },
                ])),
                #[expect(clippy::panic, reason = "just a mock")]
                _ => panic!(),
            },
            MockSetting::Mixed => Ok(Outputs(vec![
                Output {
                    name: OutputName::new("eDP-1".into()),
                    active: true,
                    primary: true,
                },
                Output {
                    name: OutputName::new("HDMI-1".into()),
                    active: true,
                    primary: false,
                },
                Output {
                    name: OutputName::new("DP-1".into()),
                    active: true,
                    primary: false,
                },
            ])),
        }
    }

    fn workspaces(&mut self) -> Result<Workspaces, Error> {
        self.check_fail()?;
        match self.setting {
            MockSetting::LaptopOnly => Ok(Workspaces(vec![
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
            ])),
            MockSetting::ExternalOnly(num) => match num {
                1 => Ok(Workspaces(vec![
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                ])),
                2 => Ok(Workspaces(vec![
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-2".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-2".into()),
                    },
                ])),
                #[expect(clippy::panic, reason = "just a mock")]
                _ => panic!(),
            },
            MockSetting::Mixed => Ok(Workspaces(vec![
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("DP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("HDMI-1".into()),
                },
            ])),
        }
    }

    fn command(&mut self, _command: Command<'_>) -> Result<(), Error> {
        self.check_fail()?;
        Ok(())
    }

    #[expect(clippy::infinite_loop, reason = "it's infallible")]
    fn subscribe(
        &mut self,
        sender: mpsc::SyncSender<Result<EventPayload, Error>>,
        debounce_time: Duration,
        _event_types: &[EventType],
    ) -> Result<Infallible, Error> {
        loop {
            sender.send(Ok(EventPayload::Output)).expect("channel open");
            thread::sleep(debounce_time);
        }
    }
}

pub struct Connection(net::UnixStream);

impl Conn for Connection {
    fn version(&mut self) -> Result<Version, Error> {
        Message::Version.send(self)?;

        let response = Response::read(self)?;

        match response {
            Response::Version(version) => Ok(version.into()),
            Response::Workspaces(_)
            | Response::Command(_)
            | Response::Outputs(_)
            | Response::Subscription(_)
            | Response::SubscriptionEvent(_) => Err(Error::UnexpectedResponse {
                expected: ResponseType::Version,
                received: response.into(),
            }),
        }
    }

    fn outputs(&mut self) -> Result<Outputs, Error> {
        Message::Outputs.send(self)?;
        let response = Response::read(self)?;

        match response {
            Response::Outputs(outputs) => Ok(outputs.into()),
            Response::Version(_)
            | Response::Workspaces(_)
            | Response::Command(_)
            | Response::Subscription(_)
            | Response::SubscriptionEvent(_) => Err(Error::UnexpectedResponse {
                expected: ResponseType::Outputs,
                received: response.into(),
            }),
        }
    }

    fn workspaces(&mut self) -> Result<Workspaces, Error> {
        Message::Workspaces.send(self)?;

        let response = Response::read(self)?;

        match response {
            Response::Workspaces(workspaces) => Ok(workspaces.into()),
            Response::Version(_)
            | Response::Command(_)
            | Response::Outputs(_)
            | Response::Subscription(_)
            | Response::SubscriptionEvent(_) => Err(Error::UnexpectedResponse {
                expected: ResponseType::Workspaces,
                received: response.into(),
            }),
        }
    }

    fn command(&mut self, command: Command<'_>) -> Result<(), Error> {
        Message::Command(command).send(self)?;

        let response = Response::read(self)?;
        match response {
            Response::Command(commands) => {
                for payload in commands {
                    if !payload.success {
                        return Err(Error::Command(
                            payload
                                .error
                                .map_or_else(|| "unknown error".into(), Into::into),
                        ));
                    }
                }
                Ok(())
            }
            Response::Version(_)
            | Response::Workspaces(_)
            | Response::Outputs(_)
            | Response::Subscription(_)
            | Response::SubscriptionEvent(_) => Err(Error::UnexpectedResponse {
                expected: ResponseType::Command,
                received: response.into(),
            }),
        }
    }

    fn subscribe(
        &mut self,
        sender: mpsc::SyncSender<Result<EventPayload, Error>>,
        debounce_time: Duration,
        event_types: &[EventType],
    ) -> Result<Infallible, Error> {
        Message::Subscribe(event_types).send(self)?;

        let response = Response::read(self)?;

        match response {
            Response::Version(_)
            | Response::Workspaces(_)
            | Response::Command(_)
            | Response::Outputs(_)
            | Response::SubscriptionEvent(_) => {
                return Err(Error::UnexpectedResponse {
                    expected: ResponseType::Subscription,
                    received: response.into(),
                });
            }
            Response::Subscription(subscription_response) => {
                if !subscription_response.success {
                    return Err(Error::ErrorResponse {
                        response_type: ResponseType::Subscription,
                        msg: match subscription_response.error {
                            Some(message) => message.into(),
                            None => "no error message".into(),
                        },
                    });
                }
            }
        }

        // disable timeout so reads block
        self.0.set_read_timeout(None)?;

        let mut event_timestamps: HashMap<EventPayloadType, Instant> = HashMap::new();

        loop {
            let response = Response::read(self)?;

            match response {
                Response::Version(_)
                | Response::Workspaces(_)
                | Response::Command(_)
                | Response::Outputs(_)
                | Response::Subscription(_) => sender
                    .send(Err(Error::UnexpectedResponse {
                        expected: ResponseType::SubscriptionEvent,
                        received: response.into(),
                    }))
                    .expect("channel open"),
                Response::SubscriptionEvent(event_payload) => {
                    let now = Instant::now();

                    let event_type: EventPayloadType = event_payload.clone().into();

                    // only emit event if there has been no event during `debounce_time`
                    match event_timestamps.entry(event_type) {
                        Entry::Occupied(mut entry) => {
                            if now.duration_since(*entry.get()) > debounce_time {
                                sender.send(Ok(event_payload)).expect("channel open");
                                let _: Instant = entry.insert(now);
                            }
                        }
                        Entry::Vacant(entry) => {
                            sender.send(Ok(event_payload)).expect("channel open");
                            let _: &mut Instant = entry.insert(now);
                        }
                    }
                }
            }
        }
    }
}

fn get_socketpath() -> Result<PathBuf, Error> {
    let cmd = process::Command::new("i3")
        .arg("--get-socketpath")
        .output()?;

    let bytes = cmd
        .stdout
        .into_iter()
        .take_while(|c| *c != b'\n')
        .collect::<Vec<u8>>();

    let string = OsStr::from_bytes(&bytes);

    let path = PathBuf::from(string);

    Ok(path)
}

pub fn connect() -> Result<Connection, Error> {
    let socketpath = get_socketpath()?;

    let socket = net::SocketAddr::from_pathname(socketpath)?;

    let stream = net::UnixStream::connect_addr(&socket)?;
    stream.set_read_timeout(Some(Duration::from_millis(100)))?;

    Ok(Connection(stream))
}

/// This takes ownership of connection because event handling
/// should have a separate connection, as it fucks with request ordering
///
/// <https://i3wm.org/docs/ipc.html#_events>
///
/// > As soon as you subscribe to an event, it is not guaranteed any longer
/// > that the requests to i3 are processed in order. This means, the
/// > following situation can happen: You send a GET_WORKSPACES request
/// > but you receive a "workspace" event before receiving the reply to
/// > GET_WORKSPACES. If your program does not want to cope which such kinds
/// > of race conditions (an event based library may not have a problem here),
/// > I suggest you create a separate connection to receive events.
pub fn start_event_listener<F, E>(
    mut connection: Connection,
    debounce_time: Duration,
    event_types: &[EventType],
    handler: F,
) -> Result<Infallible, E>
where
    F: Fn(EventPayload) -> Result<(), E> + Send,
    E: From<Error> + Send,
{
    let (tx, rx) = mpsc::sync_channel(0);

    thread::scope(|scope| -> Result<(), E> {
        let event_subscriber =
            scope.spawn(move || connection.subscribe(tx, debounce_time, event_types));

        let event_handler = scope.spawn(move || -> Result<(), E> {
            for event in rx {
                handler(event?)?;
            }

            Ok(())
        });

        event_handler.join().expect("thread to not panic")?;
        let Err(err) = event_subscriber.join().expect("thread to not panic");

        Err(err.into())
    })?;

    unreachable!()
}

#[derive(Debug, serde::Deserialize)]
struct OutputPayload {
    name: String,
    active: bool,
    primary: bool,
}

#[derive(Debug)]
pub struct Output {
    pub name: OutputName,
    pub active: bool,
    pub primary: bool,
}

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if self.active {
            write!(f, " [active]")?;
        }
        if self.primary {
            write!(f, " [primary]")?;
        }
        Ok(())
    }
}

impl From<OutputPayload> for Output {
    fn from(value: OutputPayload) -> Self {
        Self {
            name: value.name.into(),
            active: value.active,
            primary: value.primary,
        }
    }
}

#[derive(Debug)]
pub struct Workspaces(Vec<Workspace>);

impl From<Vec<WorkspacePayload>> for Workspaces {
    fn from(value: Vec<WorkspacePayload>) -> Self {
        Self(value.into_iter().map(Into::into).collect())
    }
}

impl IntoIterator for Workspaces {
    type Item = Workspace;
    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Deref for Workspaces {
    type Target = [Workspace];

    fn deref(&self) -> &[Workspace] {
        &self.0
    }
}
impl DerefMut for Workspaces {
    fn deref_mut(&mut self) -> &mut [Workspace] {
        &mut self.0
    }
}

#[derive(Debug)]
pub struct Outputs(Vec<Output>);

impl From<Vec<OutputPayload>> for Outputs {
    fn from(value: Vec<OutputPayload>) -> Self {
        Self(
            value
                .into_iter()
                .filter(|output| output.name != "xroot-0")
                .map(Into::into)
                .collect(),
        )
    }
}

impl IntoIterator for Outputs {
    type Item = Output;
    type IntoIter = <Vec<Output> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Deref for Outputs {
    type Target = [Output];

    fn deref(&self) -> &[Output] {
        &self.0
    }
}
impl DerefMut for Outputs {
    fn deref_mut(&mut self) -> &mut [Output] {
        &mut self.0
    }
}

impl Index<usize> for Outputs {
    type Output = Output;

    #[expect(
        clippy::indexing_slicing,
        reason = "transparent slicing, panicking is ok"
    )]
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Serialize)]
pub enum EventType {
    #[serde(rename = "output")]
    Output,
    #[serde(rename = "workspace")]
    Workspace,
}

enum Message<'input> {
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

        message.extend_from_slice(b"i3-ipc");
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

    fn send(self, socket: &mut Connection) -> Result<(), Error> {
        let message = self.bytes()?;
        socket.0.write_all(&message)?;
        Ok(())
    }
}

#[derive(Debug, serde::Deserialize)]
#[expect(dead_code, reason = "external data defintion")]
struct VersionPayload {
    human_readable: String,
    loaded_config_file_name: String,
    major: usize,
    minor: usize,
    patch: usize,
}

pub struct Version {
    minor: usize,
    patch: usize,
    major: usize,
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
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
#[expect(dead_code, reason = "external data defintion")]
struct WorkspacePayload {
    id: usize,
    num: usize,
    name: String,
    output: String,
}

#[derive(Debug, serde::Deserialize)]
struct CommandPayload {
    success: bool,
    error: Option<String>,
}

#[derive(Debug)]
pub struct Workspace {
    pub num: WorkspaceNumber,
    pub name: WorkspaceName,
    pub output: OutputName,
}

impl fmt::Display for Workspace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} on {}", self.num, self.output)
    }
}

impl From<WorkspacePayload> for Workspace {
    fn from(value: WorkspacePayload) -> Self {
        Self {
            num: WorkspaceNumber(value.num),
            name: WorkspaceName(value.name),
            output: OutputName(value.output),
        }
    }
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
struct SubscriptionPayload {
    success: bool,
    error: Option<String>,
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
enum Response {
    Version(VersionPayload),
    Workspaces(Vec<WorkspacePayload>),
    Command(Vec<CommandPayload>),
    Outputs(Vec<OutputPayload>),
    Subscription(SubscriptionPayload),
    SubscriptionEvent(EventPayload),
}

impl Response {
    fn read(stream: &mut Connection) -> Result<Self, Error> {
        let mut response = vec![
            0;
            "i3-ipc".chars().count().checked_add(4 + 4).ok_or_else(|| {
                Error::Protocol("payload length overflowed".into())
            })?
        ];

        stream.0.read_exact(&mut response)?;

        if &response
            .get(0..6)
            .ok_or_else(|| Error::Protocol("response too short for even the magic string".into()))?
            != b"i3-ipc"
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

        stream.0.read_exact(&mut response)?;

        // highest bit indicates event response
        if response_command >> 31 == 1 {
            let response_command = response_command & !(1 << 31);
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
