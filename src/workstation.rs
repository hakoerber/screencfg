use super::{Error, Output, OutputClass, OutputConnectionState, nonempty::NonEmptyVec};

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Workstation<'out> {
    pub(crate) laptop: Option<&'out Output>,
    pub(crate) externals: Option<NonEmptyVec<&'out Output>>,
    pub(crate) disconnected_externals: Vec<&'out Output>,
}

impl<'out> TryFrom<&'out [Output]> for Workstation<'out> {
    type Error = Error;

    fn try_from(value: &'out [Output]) -> Result<Self, Self::Error> {
        let (mut laptops, mut non_laptops): (Vec<_>, Vec<_>) = value
            .iter()
            .partition(|output| output.class == OutputClass::Laptop);

        non_laptops.sort();

        let laptop = match laptops.len() {
            0 => None,
            1 => Some(laptops.remove(0)),
            _ => {
                return Err(Error::Workstation(
                    "found more than one laptop screen".into(),
                ));
            }
        };

        let (connected_externals, disconnected_externals): (Vec<_>, Vec<_>) = non_laptops
            .into_iter()
            .partition(|output| output.connection_state == OutputConnectionState::Connected);

        let (externals, rest): (Vec<_>, Vec<_>) = connected_externals
            .into_iter()
            .partition(|output| output.class == OutputClass::External);

        if laptop.is_none() && externals.is_empty() {
            return Err(Error::Workstation("no screens found".into()));
        }

        let externals = match externals.len() {
            0 => None,
            _ => Some(NonEmptyVec::new(externals)),
        };

        if !rest.is_empty() {
            return Err(Error::Generic(
                "screens that are neither External nor Laptop found".into(),
            ));
        }

        Ok(Self {
            laptop,
            externals,
            disconnected_externals,
        })
    }
}
