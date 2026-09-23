use std::{fmt, io, path::PathBuf};

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
    #[error("apply failed: {0}")]
    Apply(Msg),
    #[error("i3: {0}")]
    I3(#[from] i3::Error),
    #[error("xrandr: {0}")]
    Xrandr(#[from] xrandr::Error),
    #[error("invalid setup: {0}")]
    InvalidSetup(Msg),
    #[error("invalid config : {0}")]
    InvalidConfig(Msg),
    #[error("could not open config file at {path}: {err}", path = path.display())]
    ConfigFileOpen { path: PathBuf, err: io::Error },
    #[error("could not find config file at {path}", path = path.display())]
    ConfigNotFound { path: PathBuf },
}
