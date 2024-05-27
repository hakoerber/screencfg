use std::{fmt, io, path::PathBuf, string};

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
            match self {
                Msg::Owned(ref s) => s.as_str(),
                Msg::Static(s) => s,
            }
        )
    }
}

#[derive(Debug)]
pub(crate) enum Error {
    Generic(Msg),
    Command(Msg),
    Classify(Msg),
    Workstation(Msg),
    Plan(Msg),
    Apply(Msg),
    I3(i3::Error),
    Xrandr(xrandr::Error),
    InvalidSetup(Msg),
    InvalidConfig(Msg),
    ConfigFileOpen(io::Error),
    ConfigNotFound { path: PathBuf },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Generic(ref msg) => format!("error: {msg}"),
                Self::Command(ref msg) => format!("command failed: {msg}"),
                Self::Classify(ref msg) => format!("classification failed: {msg}"),
                Self::Workstation(ref msg) => format!("workstation failed: {msg}"),
                Self::Plan(ref msg) => format!("plan failed: {msg}"),
                Self::Apply(ref msg) => format!("apply failed: {msg}"),
                Self::I3(ref e) => format!("i3: {e}"),
                Self::Xrandr(ref e) => format!("xrandr: {e}"),
                Self::InvalidSetup(ref msg) => format!("invalid setup: {msg}"),
                Self::InvalidConfig(ref msg) => format!("invalid config: {msg}"),
                Self::ConfigFileOpen(ref err) => format!("could not open config: {err}"),
                Self::ConfigNotFound { ref path } =>
                    format!("could not find config file at {}", path.display()),
            },
        )
    }
}

impl From<fmt::Error> for Error {
    fn from(value: fmt::Error) -> Self {
        Self::Generic(Msg::Owned(value.to_string()))
    }
}

impl From<i3::Error> for Error {
    fn from(value: i3::Error) -> Self {
        Self::I3(value)
    }
}

impl From<xrandr::Error> for Error {
    fn from(value: xrandr::Error) -> Self {
        Self::Xrandr(value)
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(value: string::FromUtf8Error) -> Self {
        Self::Command(Msg::Owned(value.to_string()))
    }
}

impl From<toml::de::Error> for Error {
    fn from(value: toml::de::Error) -> Self {
        Self::InvalidConfig(Msg::Owned(value.to_string()))
    }
}

impl std::error::Error for Error {}
