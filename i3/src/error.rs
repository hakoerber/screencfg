use std::{fmt, io};

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
    Connection(Msg),
    Command(Msg),
    Protocol(Msg),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Connection(ref msg) => format!("connection failed: {msg}"),
                Self::Command(ref msg) => format!("command failed: {msg}"),
                Self::Protocol(ref msg) => format!("overflow: {msg}"),
            }
        )
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Command(Msg::Owned(value.to_string()))
    }
}

impl From<serde_json::Error> for Error {
    fn from(value: serde_json::Error) -> Self {
        Self::Connection(Msg::Owned(value.to_string()))
    }
}

impl std::error::Error for Error {}
