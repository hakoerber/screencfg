use std::{process, string::String};

mod error;

pub use error::Error;

pub struct OutputName(String);

impl OutputName {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn into_string(self) -> String {
        self.0
    }
}

pub enum OutputState {
    Connected,
    Disconnected,
}

impl TryFrom<&str> for OutputState {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "connected" => Ok(Self::Connected),
            "disconnected" => Ok(Self::Disconnected),
            _ => Err(Error::Parse(format!(
                "unknown xrandr output state: {value}"
            ))),
        }
    }
}

pub struct Output {
    pub name: OutputName,
    pub state: OutputState,
}

impl Output {
    pub fn findall() -> Result<Vec<Self>, Error> {
        String::from_utf8(
            process::Command::new("xrandr")
                .arg("--query")
                .output()
                .map_err(|err| Error::Command(err.to_string()))?
                .stdout,
        )
        .map_err(|err| Error::Command(err.to_string()))?
        .lines()
        .skip(1) // skip header
        .filter(|line| line.chars().next().is_some_and(char::is_alphanumeric))
        .map(|line| {
            let mut parts = line.split_whitespace();
            match (parts.next(), parts.next()) {
                (Some(part_1), Some(part_2)) => Ok(Self {
                    name: OutputName(part_1.to_owned()),
                    state: part_2.try_into()?,
                }),
                _ => Err(Error::Command(format!(
                    "not enough output information in line: {line}"
                ))),
            }
        })
        .collect()
    }
}
