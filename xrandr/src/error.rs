use thiserror::Error;

use super::OutputNameError;

#[derive(Debug, Error)]
#[error("unknown xrandr output state: {value}")]
pub struct OutputStateParseError {
    pub(crate) value: String,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("command invocation failed: {0}")]
    CommandInvocation(String),
    #[error("command output was not valid utf-8: {0}")]
    CommandUtf8(String),
    #[error("line in command output failed: {message} ({line})")]
    CommandOutputLineFormat { line: String, message: &'static str },
    #[error("parsing command output failed: {0}")]
    OutputStateParse(#[from] OutputStateParseError),
    #[error("validation failed: {0}")]
    OutputNameValidation(#[from] OutputNameError),
}
