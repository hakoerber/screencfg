use std::{borrow::Borrow as _, fmt};

use nutype::nutype;

use i3::Conn as _;

use super::Error;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Output {
    pub(crate) class: Class,
    pub(crate) name: Name,
    pub(crate) connection_state: ConnectionState,
}

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.class)
    }
}

impl TryFrom<i3::Output> for Output {
    type Error = Error;

    fn try_from(value: i3::Output) -> Result<Self, Self::Error> {
        let name = value.name.try_into()?;

        let class = Class::try_detect(&name)?;

        Ok(Self {
            name,
            class,
            // all outputs detected by i3 are implicitly connected
            connection_state: ConnectionState::Connected,
        })
    }
}

impl TryFrom<xrandr::Output> for Output {
    type Error = Error;

    fn try_from(value: xrandr::Output) -> Result<Self, Self::Error> {
        let name = value.name.try_into()?;

        let class = Class::try_detect(&name)?;

        Ok(Self {
            name,
            class,
            connection_state: value.state.into(),
        })
    }
}

impl<'out> Output {
    pub(crate) fn on(&'out self) -> Setting<'out> {
        assert!(
            self.connection_state == ConnectionState::Connected,
            "tried to activate disconnected output"
        );
        Setting {
            output: self,
            state: State::Connected(ActiveState::On),
        }
    }

    pub(crate) fn off(&'out self) -> Setting<'out> {
        let state = if self.connection_state == ConnectionState::Disconnected {
            State::Disconnected
        } else {
            State::Connected(ActiveState::Off)
        };

        Setting {
            output: self,
            state,
        }
    }

    pub(crate) async fn findall(i3: &mut i3::Connection) -> Result<Vec<Self>, Error> {
        let i3_outputs = i3
            .outputs()
            .await?
            .into_iter()
            .filter(|output| output.active)
            .map(TryInto::try_into)
            .collect::<Result<Vec<Self>, Error>>()?;

        let xrandr_outputs = xrandr::Output::findall()?
            .into_iter()
            .map(TryInto::try_into)
            .collect::<Result<Vec<Self>, Error>>()?;

        let mut outputs = i3_outputs;

        for xrandr_output in xrandr_outputs {
            match outputs
                .iter_mut()
                .find(|output| output.name == xrandr_output.name)
            {
                Some(existing) => {
                    if existing.connection_state != xrandr_output.connection_state {
                        // if there is a connection state mismatch, we go with i3, as xrandr may still
                        // have inactive outputs maked as active
                    }
                }
                // if i3 does not know about the output, we use the xrandr state as-is
                None => {
                    outputs.push(xrandr_output);
                }
            }
        }

        Ok(outputs)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Class {
    Laptop,
    External,
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Laptop => "Laptop",
                Self::External => "External",
            }
        )
    }
}

impl Class {
    fn try_detect(value: &Name) -> Result<Self, Error> {
        if value.as_str().starts_with("eDP-") {
            Ok(Self::Laptop)
        } else if value.as_str().starts_with("DP-")
            || value.as_str().starts_with("HDMI-")
            || value.as_str().starts_with("DisplayPort-")
        {
            Ok(Self::External)
        } else {
            Err(Error::Classify(
                format!("could not classify output: {value}").into(),
            ))
        }
    }
}

#[nutype(
    derive(Debug, PartialEq, Eq, PartialOrd, Ord, AsRef, Display, Borrow),
    validate(not_empty)
)]
pub(crate) struct Name(String);

impl Name {
    pub(crate) fn len(&self) -> usize {
        self.as_ref().len()
    }

    pub(crate) fn as_str(&self) -> &str {
        self.as_ref()
    }
}

impl TryFrom<xrandr::OutputName> for Name {
    type Error = NameError;

    fn try_from(value: xrandr::OutputName) -> Result<Self, Self::Error> {
        Self::try_new(value.into_inner())
    }
}

impl TryFrom<i3::OutputName> for Name {
    type Error = NameError;

    fn try_from(value: i3::OutputName) -> Result<Self, Self::Error> {
        Self::try_new(value.into_inner())
    }
}

impl TryFrom<Name> for i3::OutputName {
    type Error = i3::OutputNameError;

    fn try_from(value: Name) -> Result<Self, Self::Error> {
        Self::try_new(value.into_inner())
    }
}

impl<'a> From<&'a Name> for i3::OutputNameRef<'a> {
    fn from(value: &'a Name) -> Self {
        Self::new(value.borrow())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ConnectionState {
    Connected,
    Disconnected,
}

impl From<xrandr::OutputState> for ConnectionState {
    fn from(value: xrandr::OutputState) -> Self {
        match value {
            xrandr::OutputState::Connected => Self::Connected,
            xrandr::OutputState::Disconnected => Self::Disconnected,
        }
    }
}

impl fmt::Display for ConnectionState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Connected => "connected",
                Self::Disconnected => "disconnected",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ActiveState {
    On,
    Off,
}

impl fmt::Display for ActiveState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::On => "on",
                Self::Off => "off",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum State {
    Connected(ActiveState),
    Disconnected,
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Connected(ref active_state) => format!("connected({active_state})"),
                Self::Disconnected => "disconnected".to_owned(),
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Setting<'out> {
    pub(crate) output: &'out Output,
    pub(crate) state: State,
}

impl fmt::Display for Setting<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "output {} to {}", self.output, self.state)
    }
}
