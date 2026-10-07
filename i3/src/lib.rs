use std::{
    ffi::OsStr,
    fmt,
    ops::{Deref, DerefMut, Index},
    os::unix::ffi::OsStrExt as _,
    path::PathBuf,
    process,
    vec::IntoIter,
};

use serde::Serialize;

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
