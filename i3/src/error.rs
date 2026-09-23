use std::{fmt, io};

use thiserror::Error;

use crate::ResponseType;

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

#[derive(Debug, Error)]
pub enum Error {
    #[error("connection failed: {0}")]
    Connection(Msg),
    #[error("received unexpected response (expected {expected}, received {received})")]
    UnexpectedResponse {
        expected: ResponseType,
        received: ResponseType,
    },
    #[error("unknown response command (event {event}): {id}")]
    UnknownResponseCommand { id: u32, event: bool },
    #[error("error response from {response_type}: {msg}")]
    ErrorResponse {
        response_type: ResponseType,
        msg: Msg,
    },
    #[error("command failed: {0}")]
    Command(Msg),
    #[error("protocol error: {0}")]
    Protocol(Msg),
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
