use std::path::Path;

use serde::{Deserialize, Serialize};

use super::Error;

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct Config {
    pub post_commands: Option<Vec<String>>,
}

pub(crate) fn from_path(path: &Path) -> Result<Option<Config>, Error> {
    let content = match std::fs::read_to_string(path) {
        Ok(p) => p,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => return Ok(None),
            _ => return Err(Error::ConfigFileOpen(e)),
        },
    };

    Ok(Some(toml::from_str(&content)?))
}
