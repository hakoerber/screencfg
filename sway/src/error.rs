use std::io;

use thiserror::Error;

use super::{OutputNameError, ResponseType, WorkspaceNameError};

#[derive(Debug, Error)]
pub enum Error {
    #[error("command to get socketpath failed: {0}")]
    GetSocketpath(io::Error),
    #[error("creating socket from socketpath failed: {0}")]
    InvalidSocketpath(io::Error),
    #[error("connection failed: {0}")]
    ConnectError(io::Error),
    #[error("sending to socket failed: {0}")]
    SendFailed(io::Error),
    #[error("reading from socket failed: {0}")]
    ReadFailed(io::Error),
    #[error("response too short for even the magic string")]
    ResponseTooShortForMagicString,
    #[error("magic string not found, instead received {instead:?}")]
    MagicStringNotFound { instead: Vec<u8> },
    #[error("not enough bytes for response length")]
    ResponseTooShortForResponseLength,
    #[error("not enough bytes for command")]
    ResponseTooShortForCommand,
    #[error("u32 overflowed usize")]
    U32Overflow,
    #[error("payload length bigger than 4 bytes")]
    PayloadLengthTooBig,
    #[error("failed parsing json response: {0}")]
    JsonParse(serde_json::Error),
    #[error("received unexpected response (expected {expected}, received {received})")]
    UnexpectedResponse {
        expected: ResponseType,
        received: ResponseType,
    },
    #[error("unknown response command (event {event}): {id}")]
    UnknownResponseCommand { id: u32, event: bool },
    #[error("error response from {response_type}: {msg}", msg = match *msg {
        Some(ref message) => message,
        None => "no error message",
    })]
    ErrorResponse {
        response_type: ResponseType,
        msg: Option<String>,
    },
    #[error("command failed: {}", match *message {
        Some(ref message) => message,
        None => "unknown error"
    })]
    Command { message: Option<String> },
    #[error("output name validation failed: {0}")]
    OutputNameValidation(#[from] OutputNameError),
    #[error("workspace name validation failed: {0}")]
    WorkspaceNameValidation(#[from] WorkspaceNameError),
}
