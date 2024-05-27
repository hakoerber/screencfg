use std::{
    borrow::Cow,
    ffi::OsStr,
    fmt,
    io::{Read, Write},
    ops::{Deref, DerefMut, Index},
    os::unix::{ffi::OsStrExt as _, net},
    path::PathBuf,
    process,
    time::Duration,
    vec::IntoIter,
};

mod error;
pub use error::Error;

pub enum Command {
    Nop,
    MoveWorkspace { id: usize, output: String },
}

impl From<&Command> for Cow<'static, str> {
    fn from(value: &Command) -> Self {
        match *value {
            Command::Nop => Cow::from("nop"),
            Command::MoveWorkspace { id, ref output } => Cow::from(format!(
                "[workspace=\"{id}\"] move workspace to output {output}"
            )),
        }
    }
}

pub trait Conn {
    fn version(&mut self) -> Result<Version, Error>;
    fn outputs(&mut self) -> Result<Outputs, Error>;
    fn workspaces(&mut self) -> Result<Workspaces, Error>;
    fn command(&mut self, command: Command) -> Result<(), Error>;
}

pub enum MockSetting {
    LaptopOnly,
    ExternalOnly(usize),
    Mixed,
}

pub struct MockConnection {
    pub fail: bool,
    pub setting: MockSetting,
}

impl MockConnection {
    fn check_fail(&self) -> Result<(), Error> {
        if self.fail {
            Err(Error::Connection("fail".into()))
        } else {
            Ok(())
        }
    }
}

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
                name: "eDP-1".into(),
                active: true,
                primary: true,
            }])),
            MockSetting::ExternalOnly(num) => match num {
                1 => Ok(Outputs(vec![Output {
                    name: "DP-1".into(),
                    active: true,
                    primary: false,
                }])),
                2 => Ok(Outputs(vec![
                    Output {
                        name: "DP-1".into(),
                        active: true,
                        primary: false,
                    },
                    Output {
                        name: "DP-2".into(),
                        active: false,
                        primary: false,
                    },
                ])),
                #[expect(clippy::panic, reason = "just a mock")]
                _ => panic!(),
            },
            MockSetting::Mixed => Ok(Outputs(vec![
                Output {
                    name: "eDP-1".into(),
                    active: true,
                    primary: true,
                },
                Output {
                    name: "HDMI-1".into(),
                    active: true,
                    primary: false,
                },
                Output {
                    name: "DP-1".into(),
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
                    num: 1,
                    name: "num1".into(),
                    output: "eDP-1".into(),
                },
                Workspace {
                    num: 1,
                    name: "num1".into(),
                    output: "eDP-1".into(),
                },
                Workspace {
                    num: 1,
                    name: "num1".into(),
                    output: "eDP-1".into(),
                },
                Workspace {
                    num: 1,
                    name: "num1".into(),
                    output: "eDP-1".into(),
                },
            ])),
            MockSetting::ExternalOnly(num) => match num {
                1 => Ok(Workspaces(vec![
                    Workspace {
                        num: 1,
                        name: "num1".into(),
                        output: "DP-1".into(),
                    },
                    Workspace {
                        num: 1,
                        name: "num1".into(),
                        output: "DP-1".into(),
                    },
                    Workspace {
                        num: 1,
                        name: "num1".into(),
                        output: "DP-1".into(),
                    },
                    Workspace {
                        num: 1,
                        name: "num1".into(),
                        output: "DP-1".into(),
                    },
                ])),
                2 => Ok(Workspaces(vec![
                    Workspace {
                        num: 1,
                        name: "num1".into(),
                        output: "DP-1".into(),
                    },
                    Workspace {
                        num: 1,
                        name: "num1".into(),
                        output: "DP-1".into(),
                    },
                    Workspace {
                        num: 1,
                        name: "num1".into(),
                        output: "DP-2".into(),
                    },
                    Workspace {
                        num: 1,
                        name: "num1".into(),
                        output: "DP-2".into(),
                    },
                ])),
                #[expect(clippy::panic, reason = "just a mock")]
                _ => panic!(),
            },
            MockSetting::Mixed => Ok(Workspaces(vec![
                Workspace {
                    num: 1,
                    name: "num1".into(),
                    output: "eDP-1".into(),
                },
                Workspace {
                    num: 1,
                    name: "num1".into(),
                    output: "eDP-1".into(),
                },
                Workspace {
                    num: 1,
                    name: "num1".into(),
                    output: "DP-1".into(),
                },
                Workspace {
                    num: 1,
                    name: "num1".into(),
                    output: "HDMI-1".into(),
                },
            ])),
        }
    }

    fn command(&mut self, _command: Command) -> Result<(), Error> {
        self.check_fail()?;
        Ok(())
    }
}

pub struct Connection(net::UnixStream);

impl Conn for Connection {
    fn version(&mut self) -> Result<Version, Error> {
        Message::Version.send(self)?;

        let response = Response::read(self)?;

        match response {
            Response::Version(version) => Ok(version.into()),
            Response::Workspaces(_) | Response::Command(_) | Response::Outputs(_) => Err(
                Error::Connection("received invalid response from i3".into()),
            ),
        }
    }

    fn outputs(&mut self) -> Result<Outputs, Error> {
        Message::Outputs.send(self)?;
        let response = Response::read(self)?;

        match response {
            Response::Outputs(outputs) => Ok(outputs.into()),
            Response::Version(_) | Response::Workspaces(_) | Response::Command(_) => Err(
                Error::Connection("received invalid response from i3".into()),
            ),
        }
    }

    fn workspaces(&mut self) -> Result<Workspaces, Error> {
        Message::Workspaces.send(self)?;

        let response = Response::read(self)?;

        match response {
            Response::Workspaces(workspaces) => Ok(workspaces.into()),
            Response::Version(_) | Response::Command(_) | Response::Outputs(_) => Err(
                Error::Connection("received invalid response from i3".into()),
            ),
        }
    }

    fn command(&mut self, command: Command) -> Result<(), Error> {
        Message::Command(command).send(self)?;

        let response = Response::read(self)?;
        match response {
            Response::Command(commands) => {
                for payload in commands {
                    if !payload.success {
                        return Err(Error::Command(match payload.error {
                            Some(err) => err.into(),
                            None => "unknown error".into(),
                        }));
                    }
                }
                Ok(())
            }
            Response::Version(_) | Response::Workspaces(_) | Response::Outputs(_) => Err(
                Error::Connection("received invalid response from i3".into()),
            ),
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

#[derive(Debug, serde::Deserialize)]
struct OutputPayload {
    name: String,
    active: bool,
    primary: bool,
}

#[derive(Debug)]
pub struct Output {
    pub name: String,
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
            name: value.name,
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

enum Message {
    Command(Command),
    Workspaces,
    Outputs,
    Version,
}

impl From<Message> for u32 {
    fn from(value: Message) -> Self {
        match value {
            Message::Command(_) => 0,
            Message::Workspaces => 1,
            Message::Outputs => 3,
            Message::Version => 7,
        }
    }
}

impl Message {
    fn bytes(self) -> Result<Vec<u8>, Error> {
        let payload: Option<Cow<'static, str>> = match self {
            Self::Command(ref command) => Some(command.into()),
            Self::Workspaces | Self::Outputs | Self::Version => None,
        };

        let mut message: Vec<u8> = vec![];
        let command_number: u32 = self.into();

        message.extend_from_slice(b"i3-ipc");
        message.extend_from_slice(
            &u32::try_from(payload.as_ref().map_or(0, |l| l.len()))
                .map_err(|_err| Error::Command("payload length bigger than 4 bytes".into()))?
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
    pub num: usize,
    pub name: String,
    pub output: String,
}

impl fmt::Display for Workspace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} on {}", self.num, self.output)
    }
}

impl From<WorkspacePayload> for Workspace {
    fn from(value: WorkspacePayload) -> Self {
        Self {
            num: value.num,
            name: value.name,
            output: value.output,
        }
    }
}

#[derive(Debug)]
enum Response {
    Version(VersionPayload),
    Workspaces(Vec<WorkspacePayload>),
    Command(Vec<CommandPayload>),
    Outputs(Vec<OutputPayload>),
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

        match response_command {
            0 => Ok(Self::Command(serde_json::from_slice(&response)?)),
            1 => Ok(Self::Workspaces(serde_json::from_slice(&response)?)),
            3 => Ok(Self::Outputs(serde_json::from_slice(&response)?)),
            7 => Ok(Self::Version(serde_json::from_slice(&response)?)),
            _ => Err(Error::Connection("unknown response type".into())),
        }
    }
}
