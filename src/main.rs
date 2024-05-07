use std::fmt;
use std::io;
use std::process;
use std::string;

use clap::{Args, Parser, ValueEnum};

#[derive(Debug)]
enum Error {
    Command(String),
    Classify(String),
    Workstation(String),
    Plan(String),
    Apply(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::Command(msg) => format!("command failed: {msg}"),
                Error::Classify(msg) => format!("classification failed: {msg}"),
                Error::Workstation(msg) => format!("workstation failed: {msg}"),
                Error::Plan(msg) => format!("plan failed: {msg}"),
                Error::Apply(msg) => format!("apply failed: {msg}"),
            },
        )
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Command(value.to_string())
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(value: string::FromUtf8Error) -> Self {
        Self::Command(value.to_string())
    }
}

impl std::error::Error for Error {}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum MonitorClass {
    Laptop,
    External,
}

impl TryFrom<&str> for MonitorClass {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.starts_with("eDP-") {
            Ok(Self::Laptop)
        } else if value.starts_with("DP-") || value.starts_with("HDMI-") {
            Ok(Self::External)
        } else {
            Err(Error::Classify(format!(
                "could not classify monitor: {value}"
            )))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Monitor {
    name: String,
    class: MonitorClass,
}

impl Monitor {
    fn on(self) -> MonitorSetting {
        MonitorSetting {
            monitor: self,
            state: MonitorState::On,
        }
    }

    fn off(self) -> MonitorSetting {
        MonitorSetting {
            monitor: self,
            state: MonitorState::Off,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Workstation {
    laptop: Option<Monitor>,
    externals: Option<(Monitor, Vec<Monitor>)>,
}

impl TryFrom<Vec<Monitor>> for Workstation {
    type Error = Error;

    fn try_from(mut value: Vec<Monitor>) -> Result<Self, Self::Error> {
        value.sort();

        let (mut laptops, non_laptops): (Vec<_>, Vec<_>) = value
            .into_iter()
            .partition(|monitor| monitor.class == MonitorClass::Laptop);

        let laptop = match laptops.len() {
            0 => None,
            1 => Some(laptops.remove(0)),
            _ => {
                return Err(Error::Workstation(
                    "found more than one laptop screen".into(),
                ))
            }
        };

        let (mut externals, rest): (Vec<_>, Vec<_>) = non_laptops
            .into_iter()
            .partition(|monitor| monitor.class == MonitorClass::External);

        if laptop.is_none() && externals.is_empty() {
            return Err(Error::Workstation("no screens found".to_string()));
        }

        let externals = match externals.len() {
            0 => None,
            _ => Some((externals.remove(0), externals)),
        };

        assert_eq!(rest.len(), 0);

        Ok(Self { laptop, externals })
    }
}

#[derive(Debug, PartialEq, Eq)]
enum MonitorState {
    On,
    Off,
}

#[derive(Debug, PartialEq, Eq)]
struct MonitorSetting {
    monitor: Monitor,
    state: MonitorState,
}

#[derive(Debug, PartialEq, Eq)]
struct Plan {
    monitors: Vec<MonitorSetting>,
}

impl Plan {
    fn command(&self) -> (String, Vec<String>) {
        let mut args = vec![];

        let mut left: Option<&MonitorSetting> = None;

        for monitor in &self.monitors {
            args.push("--output".into());
            args.push(monitor.monitor.name.clone());

            match monitor.state {
                MonitorState::On => {
                    args.push("--auto".into());
                    if let Some(left) = left {
                        args.push("--right-of".into());
                        args.push(left.monitor.name.clone());
                    }
                    left = Some(monitor);
                }
                MonitorState::Off => args.push("--off".into()),
            };
        }

        ("xrandr".into(), args)
    }

    fn apply(self) -> Result<(String, Vec<String>), Error> {
        let (program, args) = self.command();
        let output = process::Command::new(&program).args(&args).output()?;

        let _ = output
            .status
            .success()
            .then_some(())
            .ok_or(Error::Apply(String::from_utf8(output.stderr)?));

        Ok((program, args))
    }
}

impl fmt::Display for Plan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let active_monitors = self
            .monitors
            .iter()
            .filter(|monitor| monitor.state == MonitorState::On)
            .collect::<Vec<_>>();

        let padding_top = active_monitors
            .iter()
            .map(|monitor| "─".repeat(monitor.monitor.name.len()))
            .collect::<Vec<_>>()
            .join("─┬─");

        let padding_bottom = active_monitors
            .iter()
            .map(|monitor| "─".repeat(monitor.monitor.name.len()))
            .collect::<Vec<_>>()
            .join("─┴─");

        writeln!(
            f,
            "┌{}┐\n┆ ╭─{padding_top}─╮ ┆",
            "┄".repeat(padding_top.chars().count() + 2 * 2 + 2)
        )?;
        writeln!(
            f,
            "┆ │ {} │ ┆",
            active_monitors
                .into_iter()
                .map(|monitor| monitor.monitor.name.clone())
                .collect::<Vec<_>>()
                .join(" │ ")
        )?;
        write!(
            f,
            "┆ ╰─{padding_bottom}─╯ ┆\n└{}┘",
            "┄".repeat(padding_bottom.chars().count() + 2 * 2 + 2)
        )?;
        Ok(())
    }
}

impl Workstation {
    fn plan(&self, setup: Setup) -> Result<Plan, Error> {
        match setup {
            Setup::LaptopLeft => match self.laptop {
                None => Err(Error::Plan("no laptop screen found".to_string())),
                Some(ref laptop) => Ok(Plan {
                    monitors: {
                        let mut v = vec![laptop.clone().on()];
                        v.append({
                            &mut match &self.externals {
                                None => {
                                    return Err(Error::Plan(
                                        "no external screens found".to_string(),
                                    ))
                                }
                                Some((ext, rest)) => {
                                    let mut v = vec![ext.clone().on()];
                                    v.append(
                                        &mut rest.iter().map(|ext| ext.clone().on()).collect(),
                                    );
                                    v
                                }
                            }
                        });
                        v
                    },
                }),
            },
            Setup::LaptopRight => match self.laptop {
                None => Err(Error::Plan("no laptop screen found".to_string())),
                Some(ref laptop) => Ok(Plan {
                    monitors: {
                        let mut v = match &self.externals {
                            None => {
                                return Err(Error::Plan("no external screens found".to_string()))
                            }
                            Some((ext, rest)) => {
                                let mut v = vec![ext.clone().on()];
                                v.append(&mut rest.iter().map(|ext| ext.clone().on()).collect());
                                v
                            }
                        };
                        v.push(laptop.clone().on());
                        v
                    },
                }),
            },
            Setup::LaptopOnly => match self.laptop {
                None => Err(Error::Plan("no laptop screen found".to_string())),
                Some(ref laptop) => Ok(Plan {
                    monitors: {
                        let mut v = vec![laptop.clone().on()];
                        v.append({
                            &mut match &self.externals {
                                None => vec![],
                                Some((ext, rest)) => {
                                    let mut v = vec![ext.clone().off()];
                                    v.append(
                                        &mut rest.iter().map(|ext| ext.clone().off()).collect(),
                                    );
                                    v
                                }
                            }
                        });
                        v
                    },
                }),
            },
            Setup::ExternalOnly => match &self.externals {
                None => Err(Error::Plan("no external screens found".to_string())),
                Some((ext, rest)) => Ok(Plan {
                    monitors: {
                        let mut v = {
                            let mut v = vec![ext.clone().on()];
                            v.append(
                                &mut rest.iter().map(|monitor| monitor.clone().on()).collect(),
                            );
                            v
                        };
                        if let Some(laptop) = &self.laptop {
                            v.push(laptop.clone().off());
                        }
                        v
                    },
                }),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum OutputState {
    Connected,
    Disconnected,
}

impl TryFrom<&str> for OutputState {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "connected" => Ok(Self::Connected),
            "disconnected" => Ok(Self::Disconnected),
            _ => Err(Error::Command(format!(
                "unknown xrandr output state: {value}"
            ))),
        }
    }
}

struct Output {
    name: String,
    state: OutputState,
}

impl Monitor {
    fn new(name: &str) -> Result<Self, Error> {
        Ok(Self {
            name: name.into(),
            class: name.try_into()?,
        })
    }

    fn findall() -> Result<Vec<Self>, Error> {
        String::from_utf8(
            process::Command::new("xrandr")
                .arg("--query")
                .output()?
                .stdout,
        )?
        .lines()
        .skip(1) // skip header
        .filter(|line| {
            if let Some(c) = line.chars().next() {
                c.is_alphabetic()
            } else {
                false
            }
        })
        .map(|line| {
            let mut parts = line.split_whitespace();
            match (parts.next(), parts.next()) {
                (Some(part_1), Some(part_2)) => Ok(Output {
                    name: part_1.to_string(),
                    state: part_2.try_into()?,
                }),
                _ => Err(Error::Command(format!(
                    "not enough output information in line: {line}"
                ))),
            }
        })
        .filter(|result| {
            result
                .as_ref()
                .map(|output| output.state == OutputState::Connected)
                .unwrap_or(true)
        })
        .flat_map(|result| result.map(|output| Self::new(&output.name)))
        .collect::<Result<Vec<Monitor>, Error>>()
        .map(|mut monitors| {
            monitors.sort();
            monitors
        })
    }
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Setup {
    LaptopLeft,
    LaptopRight,
    LaptopOnly,
    ExternalOnly,
}

#[derive(Clone, Debug, Args)]
#[group(multiple = false, required = true)]
struct Approach {
    #[arg(long)]
    setup: Option<Setup>,

    #[arg(long)]
    best: bool,
}

#[derive(Debug, Parser)]
#[command(version, about)]
struct Cli {
    #[command(flatten)]
    approach: Approach,

    #[arg(long)]
    dry_run: bool,

    #[arg(long)]
    diagram: bool,
}

fn run() -> Result<(), Error> {
    let args = Cli::parse();

    let monitors = Monitor::findall()?;
    let workstation: Workstation = monitors.try_into()?;

    let plan = if let Some(setup) = args.approach.setup {
        workstation.plan(setup)?
    } else {
        workstation
            .plan(Setup::LaptopLeft)
            .or_else(|_| workstation.plan(Setup::LaptopOnly))
            .or_else(|_| workstation.plan(Setup::ExternalOnly))
            .map_err(|_| Error::Plan("no plan fit with \"best\" strategy".to_string()))?
    };

    if args.diagram {
        println!("{plan}");
    }

    let command = if args.dry_run {
        plan.command()
    } else {
        plan.apply()?
    };

    println!("{} {}", command.0, command.1.join(" "));

    Ok(())
}

fn main() -> process::ExitCode {
    process::ExitCode::from(match run() {
        Ok(()) => 0,
        Err(e) => {
            println!("{e}");
            1
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    enum PlanExpect<'a> {
        Error,
        Valid(Plan, &'a str),
    }

    #[test]
    fn single_laptop() -> Result<(), Error> {
        let laptop = Monitor {
            name: "eDP-1".to_string(),
            class: MonitorClass::Laptop,
        };

        let monitors = vec![laptop.clone()];

        let workstation: Workstation = monitors.try_into()?;

        assert_eq!(
            &workstation,
            &Workstation {
                laptop: Some(laptop.clone()),
                externals: None
            }
        );

        for (setup, expect) in [
            (Setup::LaptopLeft, PlanExpect::Error),
            (Setup::LaptopRight, PlanExpect::Error),
            (
                Setup::LaptopOnly,
                PlanExpect::Valid(
                    Plan {
                        monitors: vec![laptop.on()],
                    },
                    "--output eDP-1 --auto",
                ),
            ),
            (Setup::ExternalOnly, PlanExpect::Error),
        ] {
            let result = workstation.plan(setup);
            match expect {
                PlanExpect::Error => assert!(result.is_err()),
                PlanExpect::Valid(plan, cmd) => {
                    assert_eq!(result?, plan);
                    assert_eq!(plan.command().1.join(" "), cmd);
                }
            }
        }

        Ok(())
    }

    #[test]
    fn multiple_laptops() -> Result<(), Error> {
        let laptop1 = Monitor {
            name: "eDP-1".to_string(),
            class: MonitorClass::Laptop,
        };

        let laptop2 = Monitor {
            name: "eDP-2".to_string(),
            class: MonitorClass::Laptop,
        };

        let monitors = vec![laptop1, laptop2];

        let workstation: Result<Workstation, Error> = monitors.try_into();

        assert!(workstation.is_err());

        Ok(())
    }

    #[test]
    fn no_screens() -> Result<(), Error> {
        let monitors = vec![];

        let workstation: Result<Workstation, Error> = monitors.try_into();

        assert!(workstation.is_err());

        Ok(())
    }

    #[test]
    fn single_external() -> Result<(), Error> {
        let external = Monitor {
            name: "DP-1".to_string(),
            class: MonitorClass::External,
        };

        let monitors = vec![external.clone()];

        let workstation: Workstation = monitors.try_into()?;

        assert_eq!(
            workstation,
            Workstation {
                laptop: None,
                externals: Some((external.clone(), vec![]))
            }
        );

        for (setup, expect) in [
            (Setup::LaptopLeft, PlanExpect::Error),
            (Setup::LaptopRight, PlanExpect::Error),
            (Setup::LaptopOnly, PlanExpect::Error),
            (
                Setup::ExternalOnly,
                PlanExpect::Valid(
                    Plan {
                        monitors: vec![external.on()],
                    },
                    "--output DP-1 --auto",
                ),
            ),
        ] {
            let result = workstation.plan(setup);
            match expect {
                PlanExpect::Error => assert!(result.is_err()),
                PlanExpect::Valid(plan, cmd) => {
                    assert_eq!(result?, plan);
                    assert_eq!(plan.command().1.join(" "), cmd);
                }
            }
        }

        Ok(())
    }

    #[test]
    fn multiple_external() -> Result<(), Error> {
        let external1 = Monitor {
            name: "DP-1".to_string(),
            class: MonitorClass::External,
        };
        let external2 = Monitor {
            name: "DP-2".to_string(),
            class: MonitorClass::External,
        };
        let external3 = Monitor {
            name: "DP-3".to_string(),
            class: MonitorClass::External,
        };

        let monitors = vec![external2.clone(), external3.clone(), external1.clone()];

        let workstation: Workstation = monitors.try_into()?;

        assert_eq!(
            workstation,
            Workstation {
                laptop: None,
                externals: Some((
                    external1.clone(),
                    vec![external2.clone(), external3.clone()]
                ))
            }
        );

        for (setup, expect) in [
            (Setup::LaptopLeft, PlanExpect::Error),
            (Setup::LaptopRight, PlanExpect::Error),
            (Setup::LaptopOnly, PlanExpect::Error),
            (
                Setup::ExternalOnly,
                PlanExpect::Valid(
                    Plan {
                        monitors: vec![
                            external1.clone().on(),
                            external2.clone().on(),
                            external3.on(),
                        ],
                    },
                    "--output DP-1 --auto --output DP-2 --auto --right-of DP-1 --output DP-3 --auto --right-of DP-2",
                ),
            ),
        ] {
            let result = workstation.plan(setup);
            match expect {
                PlanExpect::Error => assert!(result.is_err()),
                PlanExpect::Valid(plan, cmd) => {
                    assert_eq!(result?, plan);
                    assert_eq!(plan.command().1.join(" "), cmd);
                }
            }
        }

        Ok(())
    }

    #[test]
    fn mixture() -> Result<(), Error> {
        let external1 = Monitor {
            name: "DP-1".to_string(),
            class: MonitorClass::External,
        };

        let external2 = Monitor {
            name: "DP-2".to_string(),
            class: MonitorClass::External,
        };

        let laptop = Monitor {
            name: "eDP-1".to_string(),
            class: MonitorClass::Laptop,
        };

        let monitors = vec![external2.clone(), laptop.clone(), external1.clone()];

        let workstation: Workstation = monitors.try_into()?;

        assert_eq!(
            workstation,
            Workstation {
                laptop: Some(laptop.clone()),
                externals: Some((external1.clone(), vec![external2.clone()]))
            }
        );

        for (setup, expect) in [
            (
                Setup::LaptopLeft,
                PlanExpect::Valid(
                    Plan {
                        monitors: vec![
                            laptop.clone().on(),
                            external1.clone().on(),
                            external2.clone().on(),
                        ],
                    },
                    "--output eDP-1 --auto --output DP-1 --auto --right-of eDP-1 --output DP-2 --auto --right-of DP-1",
                ),
            ),
            (
                Setup::LaptopRight,
                PlanExpect::Valid(
                    Plan {
                        monitors: vec![
                            external1.clone().on(),
                            external2.clone().on(),
                            laptop.clone().on(),
                        ],
                    },
                    "--output DP-1 --auto --output DP-2 --auto --right-of DP-1 --output eDP-1 --auto --right-of DP-2",
                ),
            ),
            (
                Setup::LaptopOnly,
                PlanExpect::Valid(
                    Plan {
                        monitors: vec![
                            laptop.clone().on(),
                            external1.clone().off(),
                            external2.clone().off(),
                        ],
                    },
                    "--output eDP-1 --auto --output DP-1 --off --output DP-2 --off",
                ),
            ),
            (
                Setup::ExternalOnly,
                PlanExpect::Valid(
                    Plan {
                        monitors: vec![
                            external1.clone().on(),
                            external2.clone().on(),
                            laptop.clone().off(),
                        ],
                    },
                    "--output DP-1 --auto --output DP-2 --auto --right-of DP-1 --output eDP-1 --off",
                ),
            ),
        ] {
            let result = workstation.plan(setup);
            match expect {
                PlanExpect::Error => assert!(result.is_err()),
                PlanExpect::Valid(plan, cmd) => {
                    assert_eq!(result?, plan);
                    assert_eq!(plan.command().1.join(" "), cmd);
                }
            }
        }

        Ok(())
    }
}
