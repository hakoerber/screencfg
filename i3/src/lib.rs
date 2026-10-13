use std::{
    ffi::OsStr,
    fmt,
    ops::{Deref, DerefMut, Index},
    os::unix::{ffi::OsStrExt as _, net::SocketAddr as StdSocketAddr},
    path::PathBuf,
    process,
    vec::IntoIter,
};

use nutype::nutype;
use serde::Serialize;
use tokio::net::unix::SocketAddr;

#[cfg(any(test, feature = "testing"))]
pub mod tests;

mod connection;
mod error;
mod message;
mod subscribe;

pub use connection::{Conn, Connection};
pub use error::Error;
pub use message::Command;
pub(crate) use message::{EventPayload, Message, Response, ResponseType};
pub(crate) use subscribe::Subscribe;
pub use subscribe::{Subscription, TaskResult};
use tokio::{io, net};

#[nutype(derive(Debug, Clone, Copy, Display))]
pub struct WorkspaceNumber(usize);

#[nutype(derive(Debug, PartialEq, Eq), validate(not_empty))]
pub struct WorkspaceName(String);

#[nutype(derive(Debug, Display), validate(not_empty))]
pub struct OutputName(String);

#[nutype(derive(Display))]
pub struct OutputNameRef<'a>(&'a String);

pub struct SocketPath(SocketAddr);

impl fmt::Display for SocketPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .as_pathname()
                .map(|path| path.display().to_string())
                .unwrap_or_else(|| "[unknown path]".to_owned())
        )
    }
}

pub fn get_socket_path() -> Result<SocketPath, Error> {
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

    let socket_addr = StdSocketAddr::from_pathname(path)?;

    Ok(SocketPath(socket_addr.into()))
}

pub async fn connect(socket_path: &SocketPath) -> Result<Connection, Error> {
    let stream = net::UnixStream::connect_addr(&socket_path.0).await?;

    let buffer = io::BufReader::new(stream);

    Ok(Connection::new(buffer))
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

#[derive(Debug)]
pub struct Workspaces(Vec<Workspace>);

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
