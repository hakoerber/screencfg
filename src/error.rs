use std::{fmt, process::ExitCode};

use super::{ordering, output, plan, workspace};

use thiserror::Error;

#[derive(Debug, Error)]
pub(crate) enum Error {
    #[error("diagram output error: {0}")]
    Diagram(fmt::Error),
    #[error("ordering parse error: {0}")]
    OrderingParse(#[from] ordering::ParseError),
    #[error("could not classify output: {name}")]
    Classify { name: output::Name },
    #[error("workstation failed: no screens found")]
    WorkstationWithoutScreens,
    #[error("workstation failed: multiple laptop screens found ({count})")]
    WorkstationWithMultipleLaptopScreens { count: usize },
    #[error("plan failed: {0}")]
    Plan(#[from] plan::Error),
    #[error("apply failed: {0}")]
    Apply(#[from] plan::ApplyError),
    #[error("no plan fit with \"best\" strategy")]
    NoBestStrategyFit,
    #[error("sway: {0}")]
    Sway(#[from] sway::Error),
    #[error("output name validation failed: {0}")]
    OutputNameValidation(#[from] output::NameError),
    #[error("workspace name validation failed: {0}")]
    WorkspaceNameValidation(#[from] workspace::NameError),
    #[error("installation of global tracing subscriber failed: {0}")]
    TracingSetGlobalDefaultSubscriber(#[from] tracing::dispatcher::SetGlobalDefaultError),
    #[error("output of workspace {workspace} ({output}) not found in sway outputs")]
    WorkspaceOutputMismatch {
        workspace: workspace::Name,
        output: output::Name,
    },
}

impl From<Error> for ExitCode {
    fn from(_value: Error) -> Self {
        Self::FAILURE
    }
}
