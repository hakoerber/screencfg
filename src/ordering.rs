use std::num::ParseIntError;
use tap::prelude::*;

use super::error::Error;

#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(crate) enum ExternalOrdering {
    Default,
    Custom { order: Vec<usize> },
}

impl ExternalOrdering {
    pub(crate) fn parse_from_str(input: &str, external_output_count: usize) -> Result<Self, Error> {
        let elems = input
            .split(',')
            .map(str::parse::<usize>)
            .collect::<Result<Vec<usize>, ParseIntError>>()
            .map_err(|err| {
                Error::Command(format!("could not parse order as integer: {err}").into())
            })?;

        if external_output_count != elems.len() {
            return Err(Error::Command(
                "custom ordering needs to be the same length as number of outputs".into(),
            ));
        }

        let sorted = elems.clone().tap_mut(|f| f.sort_unstable());

        if sorted != (1..=(elems.len())).collect::<Vec<usize>>() {
            return Err(Error::Command(
                "custom ordering needs to contain incrementing integers only".into(),
            ));
        }

        Ok(Self::Custom { order: elems })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub(crate) fn test_order_parsing() {
        const KNOWN_GOOD: &str = "1,2,3";

        assert_eq!(
            ExternalOrdering::parse_from_str(KNOWN_GOOD, 3).unwrap(),
            ExternalOrdering::Custom {
                order: vec![1, 2, 3]
            }
        );

        let _err = ExternalOrdering::parse_from_str("1,2,x", 3).unwrap_err();
        let _err = ExternalOrdering::parse_from_str("1,2;3", 3).unwrap_err();
        let _err = ExternalOrdering::parse_from_str("1,2,4", 3).unwrap_err();
        let _err = ExternalOrdering::parse_from_str("4,2,1", 3).unwrap_err();
        let _err = ExternalOrdering::parse_from_str("0,1,2", 3).unwrap_err();

        let _err = ExternalOrdering::parse_from_str(KNOWN_GOOD, 2).unwrap_err();
        let _err = ExternalOrdering::parse_from_str(KNOWN_GOOD, 0).unwrap_err();
    }
}
