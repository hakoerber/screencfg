use tap::Tap as _;

use nonempty::NonEmptyVec;

use super::{Error, Output, OutputClass, ordering::MonitorOrdering, output};

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) enum UnorderedWorkstation<'out> {
    LaptopOnly {
        laptop: &'out Output,
    },
    MonitorOnly {
        monitors: NonEmptyVec<&'out Output>,
    },
    Mixed {
        laptop: &'out Output,
        monitors: NonEmptyVec<&'out Output>,
    },
}

#[derive(Debug)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
pub(crate) enum Workstation<'out> {
    LaptopOnly {
        laptop: &'out Output,
    },
    MonitorOnly {
        monitors: NonEmptyVec<&'out Output>,
    },
    Mixed {
        laptop: &'out Output,
        monitors: NonEmptyVec<&'out Output>,
    },
}

impl<'out> UnorderedWorkstation<'out> {
    pub(crate) fn from_outputs(value: &'out [Output]) -> Result<Self, Error> {
        let (mut laptops, non_laptops): (Vec<_>, Vec<_>) = value
            .iter()
            .partition(|output| output.class == OutputClass::Laptop);

        tracing::debug!(
            "found {} laptop and {} non-laptop outputs",
            laptops.len(),
            non_laptops.len()
        );

        let laptop = match laptops.len() {
            0 => None,
            1 => Some(laptops.remove(0)),
            _ => {
                return Err(Error::Workstation(
                    "found more than one laptop screen".into(),
                ));
            }
        };

        tracing::debug!("laptop: {laptop:?}");

        let (monitors, rest): (Vec<_>, Vec<_>) = non_laptops
            .into_iter()
            .partition(|output| output.class == OutputClass::Monitor);

        let monitors = match monitors.len() {
            0 => None,
            _ => Some(NonEmptyVec::new(monitors).expect("checked for len explicitly")),
        };

        if !rest.is_empty() {
            return Err(Error::Generic(
                "screens that are neither Monitor nor Laptop found".into(),
            ));
        }

        match (laptop, monitors) {
            (None, None) => Err(Error::Workstation("no screens found".into())),
            (None, Some(monitors)) => Ok(Self::MonitorOnly { monitors }),
            (Some(laptop), None) => Ok(Self::LaptopOnly { laptop }),
            (Some(laptop), Some(monitors)) => Ok(Self::Mixed { laptop, monitors }),
        }
    }

    pub(crate) fn order(mut self, monitor_ordering: &MonitorOrdering) -> Workstation<'out> {
        match self {
            Self::LaptopOnly { laptop: _ } => (),
            Self::MonitorOnly { ref mut monitors }
            | Self::Mixed {
                laptop: _,
                ref mut monitors,
            } => {
                // monitors are implicitly ordered by their name, from left to right. this keeps them stable, regardless
                // of sway output
                let new = monitors
                    .clone()
                    .tap_mut(|outputs| outputs.sort_by_key(|output| output.name.as_str()));

                // but if there is a requested explicit ordering, we shuffle them around to match that
                let new = match *monitor_ordering {
                    MonitorOrdering::Default => new,
                    MonitorOrdering::Custom { ref order } => {
                        let mut out = Vec::with_capacity(new.len());
                        assert_eq!(
                            order.len(),
                            monitors.len(),
                            "assured during creation of order"
                        );

                        for order in order {
                            out.push(
                                *monitors
                                    .get(order.checked_sub(1).expect("order is always positive"))
                                    .expect("order contains incrementing integers"),
                            );
                        }

                        NonEmptyVec::new(out).expect("derived from nonempty vec")
                    }
                };

                *monitors = new;
            }
        }

        match self {
            UnorderedWorkstation::LaptopOnly { laptop } => Workstation::LaptopOnly { laptop },
            Self::MonitorOnly { monitors } => Workstation::MonitorOnly { monitors },
            Self::Mixed { laptop, monitors } => Workstation::Mixed { laptop, monitors },
        }
    }
}

impl Workstation<'_> {
    pub(crate) fn max_output_height(&self) -> output::Height {
        match *self {
            Workstation::LaptopOnly { laptop } => laptop.mode.height,
            Workstation::MonitorOnly { ref monitors } => monitors
                .iter()
                .map(|output| output.mode.height)
                .max()
                .expect("nonempty vec"),
            Workstation::Mixed {
                laptop,
                ref monitors,
            } => std::iter::once(&laptop)
                .chain(monitors)
                .map(|output| output.mode.height)
                .max()
                .expect("once (plus nonempty vec)"),
        }
    }
}
