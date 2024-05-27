use std::ffi::OsStr;
use std::fmt;
use std::io::{self, Read, Write};
use std::ops::{Deref, DerefMut, Index};
use std::os::unix::ffi::OsStrExt as _;
use std::os::unix::net;
use std::path::PathBuf;
use std::process;
use std::time::Duration;

#[derive(Debug)]
pub enum Error {
    Connection(String),
    Command(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::Connection(msg) => format!("connection failed: {msg}"),
                Error::Command(msg) => format!("command failed: {msg}"),
            }
        )
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Command(value.to_string())
    }
}

impl From<serde_json::Error> for Error {
    fn from(value: serde_json::Error) -> Self {
        Self::Connection(value.to_string())
    }
}

impl std::error::Error for Error {}

#[derive(Clone)]
pub enum Command {
    Nop,
    MoveWorkspace { id: usize, output: String },
}

impl From<Command> for String {
    fn from(value: Command) -> Self {
        match value {
            Command::Nop => "nop".to_string(),
            Command::MoveWorkspace { id, output } => {
                format!("[workspace=\"{id}\"] move workspace to output {output}")
            }
        }
    }
}

#[allow(dead_code)]
pub trait Conn {
    fn version(&mut self) -> Result<Version, Error>;
    fn outputs(&mut self) -> Result<Outputs, Error>;
    fn workspaces(&mut self) -> Result<Workspaces, Error>;
    fn command(&mut self, command: Command) -> Result<(), Error>;
}

#[cfg(test)]
pub enum MockSetting {
    LaptopOnly,
    ExternalOnly(usize),
    Mixed,
}

#[cfg(test)]
pub struct MockConnection {
    pub fail: bool,
    pub setting: MockSetting,
}

#[cfg(test)]
impl MockConnection {
    fn check_fail(&self) -> Result<(), Error> {
        if self.fail {
            Err(Error::Connection("fail".into()))
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
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
            _ => Err(Error::Connection(
                "received invalid response from i3".into(),
            )),
        }
    }

    fn outputs(&mut self) -> Result<Outputs, Error> {
        Message::Outputs.send(self)?;
        let response = Response::read(self)?;

        match response {
            Response::Outputs(outputs) => Ok(outputs.into()),
            _ => Err(Error::Connection(
                "received invalid response from i3".into(),
            )),
        }
    }

    fn workspaces(&mut self) -> Result<Workspaces, Error> {
        Message::Workspaces.send(self)?;

        let response = Response::read(self)?;

        match response {
            Response::Workspaces(workspaces) => Ok(workspaces.into()),
            _ => Err(Error::Connection(
                "received invalid response from i3".into(),
            )),
        }
    }

    fn command(&mut self, command: Command) -> Result<(), Error> {
        Message::Command(command).send(self)?;

        let response = Response::read(self)?;
        match response {
            Response::Command(commands) => {
                for payload in commands {
                    if !payload.success {
                        return Err(Error::Command(
                            payload.error.unwrap_or_else(|| "unknown error".into()),
                        ));
                    }
                }
                Ok(())
            }
            _ => Err(Error::Connection(
                "received invalid response from i3".into(),
            )),
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

#[derive(Clone, Debug)]
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
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Deref for Workspaces {
    type Target = [Workspace];

    fn deref(&self) -> &[Workspace] {
        &self.0[..]
    }
}
impl DerefMut for Workspaces {
    fn deref_mut(&mut self) -> &mut [Workspace] {
        &mut self.0[..]
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
        &self.0[..]
    }
}
impl DerefMut for Outputs {
    fn deref_mut(&mut self) -> &mut [Output] {
        &mut self.0[..]
    }
}

impl Index<usize> for Outputs {
    type Output = Output;

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
        let payload: Option<String> = match self {
            Message::Command(ref command) => Some(command.clone().into()),
            Message::Workspaces | Message::Outputs | Message::Version => None,
        };

        let mut message: Vec<u8> = vec![];
        let command_number: u32 = self.into();

        message.extend_from_slice("i3-ipc".as_bytes());
        message.extend_from_slice(
            &u32::try_from(payload.as_ref().map_or(0, String::len))
                .map_err(|_| Error::Command("payload length bigger than 4 bytes".into()))?
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

#[allow(dead_code)]
#[derive(Debug, serde::Deserialize)]
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
struct WorkspacePayload {
    #[allow(dead_code)]
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
    #[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Debug)]
enum Response {
    Version(VersionPayload),
    Workspaces(Vec<WorkspacePayload>),
    Command(Vec<CommandPayload>),
    Outputs(Vec<OutputPayload>),
}

impl Response {
    fn read(stream: &mut Connection) -> Result<Self, Error> {
        let mut response = vec![0; "i3-ipc".chars().count() + 4 + 4];

        stream.0.read_exact(&mut response)?;

        assert_eq!(&response[0..6], "i3-ipc".as_bytes());
        let response_length = u32::from_ne_bytes(response[6..10].try_into().unwrap());
        let response_command = u32::from_ne_bytes(response[10..14].try_into().unwrap());

        response = vec![0; response_length as usize];

        stream.0.read_exact(&mut response)?;

        match response_command {
            0 => Ok(Response::Command(serde_json::from_slice(&response)?)),
            1 => Ok(Response::Workspaces(serde_json::from_slice(&response)?)),
            3 => Ok(Response::Outputs(serde_json::from_slice(&response)?)),
            7 => Ok(Response::Version(serde_json::from_slice(&response)?)),
            _ => Err(Error::Connection("unknown response type".into())),
        }
    }
}
