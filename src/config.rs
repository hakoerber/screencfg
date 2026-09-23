use std::path::Path;

use serde::Deserialize;

use super::Error;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct Config {
    pub post_commands: Option<Vec<String>>,
}

pub(crate) fn from_path(path: &Path) -> Result<Option<Config>, Error> {
    let content = match std::fs::read_to_string(path) {
        Ok(p) => p,
        Err(err) => match err.kind() {
            std::io::ErrorKind::NotFound => return Ok(None),
            _ => {
                return Err(Error::ConfigFileOpen {
                    path: path.to_owned(),
                    err,
                });
            }
        },
    };

    Ok(Some(toml::from_str(&content).map_err(|err| {
        Error::InvalidConfig(err.to_string().into())
    })?))
}
