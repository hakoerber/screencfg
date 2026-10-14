use std::num::ParseIntError;
use tap::prelude::*;
use thiserror::Error;

#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(crate) enum MonitorOrdering {
    Default,
    Custom { order: Vec<usize> },
}

#[derive(Debug, Error)]
#[expect(
    variant_size_differences,
    reason = "two usizes (16 bytes usually) are still acceptable"
)]
pub(crate) enum ParseError {
    #[error("parsing ordering failed: not an integer: {0}")]
    OrderNotAnInteger(ParseIntError),
    #[error(
        "custom ordering (length {length}) needs to be the same length as number of monitors ({outputs})"
    )]
    OrderLengthMismatch { length: usize, outputs: usize },
    #[error("custom ordering needs to contain incrementing integers only")]
    NonIncrementing,
}

impl MonitorOrdering {
    pub(crate) fn parse_from_str(
        input: &str,
        monitor_output_count: usize,
    ) -> Result<Self, ParseError> {
        let elems = input
            .split(',')
            .map(str::parse::<usize>)
            .collect::<Result<Vec<usize>, ParseIntError>>()
            .map_err(|err| ParseError::OrderNotAnInteger(err))?;

        if monitor_output_count != elems.len() {
            return Err(ParseError::OrderLengthMismatch {
                length: elems.len(),
                outputs: monitor_output_count,
            });
        }

        let sorted = elems.clone().tap_mut(|f| f.sort_unstable());

        if sorted != (1..=(elems.len())).collect::<Vec<usize>>() {
            return Err(ParseError::NonIncrementing);
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
            MonitorOrdering::parse_from_str(KNOWN_GOOD, 3).unwrap(),
            MonitorOrdering::Custom {
                order: vec![1, 2, 3]
            }
        );

        let _err = MonitorOrdering::parse_from_str("1,2,x", 3).unwrap_err();
        let _err = MonitorOrdering::parse_from_str("1,2;3", 3).unwrap_err();
        let _err = MonitorOrdering::parse_from_str("1,2,4", 3).unwrap_err();
        let _err = MonitorOrdering::parse_from_str("4,2,1", 3).unwrap_err();
        let _err = MonitorOrdering::parse_from_str("0,1,2", 3).unwrap_err();

        let _err = MonitorOrdering::parse_from_str(KNOWN_GOOD, 2).unwrap_err();
        let _err = MonitorOrdering::parse_from_str(KNOWN_GOOD, 0).unwrap_err();
    }
}
