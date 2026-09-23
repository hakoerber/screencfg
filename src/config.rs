use std::{
    env,
    path::{Path, PathBuf},
};

use serde::Deserialize;

use super::Error;

const XDG_CONFIG_HOME: &str = "XDG_CONFIG_HOME";

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

pub(crate) fn find(path: Option<&Path>) -> Result<Option<Config>, Error> {
    match path {
        Some(path) => match from_path(path)? {
            Some(c) => Ok(Some(c)),
            None => Err(Error::ConfigNotFound {
                path: path.to_owned(),
            }),
        },
        None => {
            let mut config_home = match env::var(XDG_CONFIG_HOME) {
                Ok(v) => Ok(PathBuf::from(v)),
                Err(e) => match e {
                    env::VarError::NotPresent => match env::var("HOME") {
                        Ok(v) => Ok([&v, ".config"].iter().collect::<PathBuf>()),
                        Err(e) => match e {
                            env::VarError::NotPresent => Err(Error::Generic("HOME not set".into())),
                            env::VarError::NotUnicode(_) => {
                                Err(Error::Generic("HOME contains invalid unicode".into()))
                            }
                        },
                    },
                    env::VarError::NotUnicode(_) => Err(Error::Generic(
                        "{XDG_CONFIG_HOME} env variable is not unicode".into(),
                    )),
                },
            }?;
            config_home.push("screencfg.toml");
            Ok(from_path(&config_home)?)
        }
    }
}
