use std::fmt;

use super::{Error, Output, OutputName};

#[derive(Debug)]
pub(crate) struct Workspaces<'out>(pub(crate) Vec<Workspace<'out>>);

impl<'out> Workspaces<'out> {
    pub(crate) fn convert(
        workspaces: i3::Workspaces,
        outputs: &[&'out Output],
    ) -> Result<Self, Error> {
        Ok(Self(
            workspaces
                .into_iter()
                .map(|from| {
                    let output_name_from_i3_workspaces: OutputName = from.output.into();
                    Ok(Workspace {
                        num: from.num.into(),
                        name: from.name.into(),
                        output: outputs
                            .iter()
                            .find(|output| output_name_from_i3_workspaces == output.name)
                            .ok_or_else(|| {
                                Error::Generic(
                                    format!(
                                        "output of workspace {} ({}) not found in i3 outputs",
                                        from.num, output_name_from_i3_workspaces
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

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct WorkspaceName(String);

impl fmt::Display for WorkspaceName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<i3::WorkspaceName> for WorkspaceName {
    fn from(value: i3::WorkspaceName) -> Self {
        Self(value.into_inner())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) struct WorkspaceNumber(pub(crate) usize);

impl fmt::Display for WorkspaceNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<WorkspaceNumber> for i3::WorkspaceNumber {
    fn from(value: WorkspaceNumber) -> Self {
        Self::new(value.0)
    }
}

impl From<i3::WorkspaceNumber> for WorkspaceNumber {
    fn from(value: i3::WorkspaceNumber) -> Self {
        Self(value.into_inner())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Workspace<'out> {
    pub(crate) num: WorkspaceNumber,
    pub(crate) name: WorkspaceName,
    pub(crate) output: &'out Output,
}

impl fmt::Display for Workspace<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {} on {}", self.num, self.name, self.output)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct WorkspaceSetting<'ws, 'out> {
    pub(crate) workspace: &'ws Workspace<'out>,
    pub(crate) output: &'out Output,
}

impl fmt::Display for WorkspaceSetting<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "workspace {} to output {}", self.workspace, self.output)
    }
}
