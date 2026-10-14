use std::{borrow::Borrow as _, fmt};

use nutype::nutype;

use sway::Conn as _;

use super::Error;

#[derive(Debug, Clone, Copy)]
pub(crate) enum VerticalVector {
    Up { start: PosY, len: usize },
}

impl VerticalVector {
    pub(crate) fn end(self) -> Option<PosY> {
        match self {
            Self::Up { start, len } => Some(PosY(start.0.checked_sub(len)?)),
        }
    }
}

#[nutype(derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq))]
pub(crate) struct Width(usize);

impl From<sway::Width> for Width {
    fn from(value: sway::Width) -> Self {
        Self::new(value.into_inner())
    }
}

#[nutype(derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq))]
pub(crate) struct Height(usize);

impl Height {
    pub(crate) fn sub(self, rhs: Self) -> VerticalVector {
        VerticalVector::Up {
            start: PosY(self.into_inner()),
            len: rhs.into_inner(),
        }
    }
}

impl From<sway::Height> for Height {
    fn from(value: sway::Height) -> Self {
        Self::new(value.into_inner())
    }
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) enum RefreshRate {
    Ms(usize),
}

impl From<sway::RefreshRate> for RefreshRate {
    fn from(value: sway::RefreshRate) -> Self {
        match value {
            sway::RefreshRate::Ms(ms) => Self::Ms(ms),
        }
    }
}

impl From<RefreshRate> for sway::RefreshRate {
    fn from(value: RefreshRate) -> Self {
        match value {
            RefreshRate::Ms(ms) => Self::Ms(ms),
        }
    }
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) struct Mode {
    pub(crate) width: Width,
    pub(crate) height: Height,
    pub(crate) refresh_rate: RefreshRate,
}

impl From<sway::OutputMode> for Mode {
    fn from(value: sway::OutputMode) -> Self {
        Self {
            width: value.width.into(),
            height: value.height.into(),
            refresh_rate: value.refresh_rate.into(),
        }
    }
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) struct Output {
    pub(crate) class: Class,
    pub(crate) name: Name,
    pub(crate) mode: Mode,
    pub(crate) state: ActiveState,
}

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.class)
    }
}

impl TryFrom<sway::Output> for Output {
    type Error = Error;

    fn try_from(value: sway::Output) -> Result<Self, Self::Error> {
        let name = value.name.try_into()?;

        let class = Class::try_detect(&name)?;

        Ok(Self {
            name,
            class,
            mode: value.mode.into(),
            state: if value.active {
                ActiveState::On {
                    position: value.position.into(),
                }
            } else {
                ActiveState::Off
            },
        })
    }
}

impl<'out> Output {
    pub(crate) fn set(&'out self, state: ActiveState) -> Setting<'out> {
        if self.state == state {
            Setting::Unchanged { output: self }
        } else {
            Setting::Change {
                output: self,
                state,
            }
        }
    }

    pub(crate) async fn findall(sway: &mut sway::Connection) -> Result<Vec<Self>, Error> {
        sway.outputs()
            .await?
            .into_iter()
            .filter(|output| output.active)
            .map(TryInto::try_into)
            .collect::<Result<Vec<Self>, Error>>()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Class {
    Laptop,
    Monitor,
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Laptop => "Laptop",
                Self::Monitor => "Monitor",
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
            Ok(Self::Monitor)
        } else {
            Err(Error::Classify {
                name: value.clone(),
            })
        }
    }
}

#[nutype(
    derive(Debug, PartialEq, Eq, PartialOrd, Ord, AsRef, Display, Borrow, Clone),
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

impl TryFrom<sway::OutputName> for Name {
    type Error = NameError;

    fn try_from(value: sway::OutputName) -> Result<Self, Self::Error> {
        Self::try_new(value.into_inner())
    }
}

impl TryFrom<Name> for sway::OutputName {
    type Error = sway::OutputNameError;

    fn try_from(value: Name) -> Result<Self, Self::Error> {
        Self::try_new(value.into_inner())
    }
}

impl<'a> From<&'a Name> for sway::OutputNameRef<'a> {
    fn from(value: &'a Name) -> Self {
        Self::new(value.borrow())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) enum ActiveState {
    On { position: Position },
    Off,
}

impl ActiveState {
    pub(crate) fn is_active(&self) -> bool {
        matches!(self, Self::On { position: _ })
    }
}

impl fmt::Display for ActiveState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::On { position } => write!(f, "on at {position}"),
            Self::Off => write!(f, "off"),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, derive_more::Display)]
pub(crate) struct PosX(pub usize);

impl PosX {
    pub(crate) fn add(&mut self, value: Self) {
        self.0 = self.0.checked_add(value.0).expect("posx overflowed usize");
    }
}

impl From<PosX> for sway::PosX {
    fn from(value: PosX) -> Self {
        Self(value.0)
    }
}

impl From<sway::PosX> for PosX {
    fn from(value: sway::PosX) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, derive_more::Display)]
pub(crate) struct PosY(pub usize);

impl From<PosY> for sway::PosY {
    fn from(value: PosY) -> Self {
        Self(value.0)
    }
}

impl From<sway::PosY> for PosY {
    fn from(value: sway::PosY) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) struct Position {
    pub x: PosX,
    pub y: PosY,
}

impl Position {
    pub(crate) fn null() -> Self {
        Self {
            x: PosX(0),
            y: PosY(0),
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}x{}", self.x, self.y)
    }
}

impl From<Position> for sway::Position {
    fn from(value: Position) -> Self {
        Self {
            x: value.x.into(),
            y: value.y.into(),
        }
    }
}

impl From<sway::Position> for Position {
    fn from(value: sway::Position) -> Self {
        Self {
            x: value.x.into(),
            y: value.y.into(),
        }
    }
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) enum Setting<'out> {
    Unchanged {
        output: &'out Output,
    },
    Change {
        output: &'out Output,
        state: ActiveState,
    },
}

impl Setting<'_> {
    pub(crate) fn output(&self) -> &Output {
        match *self {
            Setting::Unchanged { output } | Setting::Change { output, state: _ } => output,
        }
    }
}

impl fmt::Display for Setting<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Setting::Unchanged { output } => {
                write!(f, "output {output} unchanged")
            }
            Setting::Change { output, ref state } => {
                write!(f, "output {output} to {state}")
            }
        }
    }
}

// impl Setting<'_> {
//     pub(crate) fn needs_change(self) -> bool {
//         self.output.state == self.state
//     }
// }
