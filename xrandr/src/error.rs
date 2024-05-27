use std::{fmt, io, string};

#[derive(Debug)]
pub enum Msg {
    Owned(String),
    Static(&'static str),
}

impl From<&'static str> for Msg {
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}

impl From<String> for Msg {
    fn from(value: String) -> Self {
        Self::Owned(value)
    }
}

impl fmt::Display for Msg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Owned(ref s) => s.as_str(),
                Self::Static(s) => s,
            }
        )
    }
}

#[derive(Debug)]
pub enum Error {
    Command(Msg),
    Parse(Msg),
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Command(Msg::Owned(value.to_string()))
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(value: string::FromUtf8Error) -> Self {
        Self::Parse(Msg::Owned(value.to_string()))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Command(ref msg) => format!("command failed: {msg}"),
                Self::Parse(ref msg) => format!("parsing command output failed: {msg}"),
            }
        )
    }
}

impl std::error::Error for Error {}
