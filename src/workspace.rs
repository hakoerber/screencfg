use std::fmt;

use nutype::nutype;

use super::{Error, Output, OutputName};

#[derive(Debug)]
pub(crate) struct Workspaces<'out>(pub(crate) Vec<Workspace<'out>>);

impl<'out> Workspaces<'out> {
    pub(crate) fn convert(
        workspaces: sway::Workspaces,
        outputs: &[&'out Output],
    ) -> Result<Self, Error> {
        Ok(Self(
            workspaces
                .into_iter()
                .map(|from| {
                    let output_name_from_sway_workspaces: OutputName = from.output.try_into()?;
                    Ok(Workspace {
                        num: from.num.into(),
                        name: from.name.try_into()?,
                        output: outputs
                            .iter()
                            .find(|output| output_name_from_sway_workspaces == output.name)
                            .ok_or_else(|| {
                                Error::Generic(
                                    format!(
                                        "output of workspace {} ({}) not found in sway outputs",
                                        from.num, output_name_from_sway_workspaces
                                    )
                                    .into(),
                                )
                            })?,
                    })
                })
                .collect::<Result<Vec<Workspace<'_>>, Error>>()?,
        ))
    }
}

#[nutype(derive(Debug, PartialEq, Eq, Display), validate(not_empty))]
pub(crate) struct Name(String);

impl TryFrom<sway::WorkspaceName> for Name {
    type Error = NameError;

    fn try_from(value: sway::WorkspaceName) -> Result<Self, Self::Error> {
        Self::try_new(value.into_inner())
    }
}

#[nutype(derive(Debug, PartialEq, Eq, Copy, Clone, Display))]
pub(crate) struct Number(usize);

impl From<Number> for sway::WorkspaceNumber {
    fn from(value: Number) -> Self {
        Self::new(value.into_inner())
    }
}

impl From<sway::WorkspaceNumber> for Number {
    fn from(value: sway::WorkspaceNumber) -> Self {
        Self::new(value.into_inner())
    }
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) struct Workspace<'out> {
    pub(crate) num: Number,
    pub(crate) name: Name,
    pub(crate) output: &'out Output,
}

impl fmt::Display for Workspace<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {} on {}", self.num, self.name, self.output)
    }
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) enum Setting<'ws, 'out> {
    Unchanged {
        workspace: &'ws Workspace<'out>,
    },
    Change {
        workspace: &'ws Workspace<'out>,
        output: &'out Output,
    },
}

impl fmt::Display for Setting<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Setting::Unchanged { workspace } => {
                write!(f, "workspace {workspace} unchanged")
            }
            Setting::Change { workspace, output } => {
                write!(f, "workspace {workspace} to {output}")
            }
        }
    }
}
