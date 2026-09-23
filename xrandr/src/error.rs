use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("command failed: {0}")]
    Command(String),
    #[error("parsing command output failed: {0}")]
    Parse(String),
}
