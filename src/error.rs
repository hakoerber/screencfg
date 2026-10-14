use std::{fmt, process::ExitCode};

use super::{output, workspace};

use thiserror::Error;

#[derive(Debug)]
pub(crate) enum Msg {
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
pub(crate) enum Error {
    #[error("error: {0}")]
    Generic(Msg),
    #[error("command failed: {0}")]
    Command(Msg),
    #[error("classification failed: {0}")]
    Classify(Msg),
    #[error("workstation failed: {0}")]
    Workstation(Msg),
    #[error("plan failed: {0}")]
    Plan(Msg),
    #[error("sway: {0}")]
    I3(#[from] sway::Error),
    #[error("invalid setup: {0}")]
    InvalidSetup(Msg),
    #[error("output name validation failed: {0}")]
    OutputNameValidation(#[from] output::NameError),
    #[error("workspace name validation failed: {0}")]
    WorkspaceNameValidation(#[from] workspace::NameError),
    #[error("installation of global tracing subscriber failed: {0}")]
    TracingSetGlobalDefaultSubscriber(#[from] tracing::dispatcher::SetGlobalDefaultError),
}

impl From<Error> for ExitCode {
    fn from(_value: Error) -> Self {
        Self::FAILURE
    }
}
