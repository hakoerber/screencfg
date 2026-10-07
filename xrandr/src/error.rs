use thiserror::Error;

use super::OutputNameError;

#[derive(Debug, Error)]
pub enum Error {
    #[error("command failed: {0}")]
    Command(String),
    #[error("parsing command output failed: {0}")]
    Parse(String),
    #[error("validation failed: {0}")]
    OutputNameValidation(#[from] OutputNameError),
}
