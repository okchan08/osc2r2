#![allow(dead_code)]
use super::{errors::ParseError, parser::SpanIterator};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub(super) enum Constraint {
    #[default]
    Keep,
    RemoveDefault,
}

impl Constraint {
    pub fn parse_constraint(_span_iter: &mut SpanIterator) -> Result<Constraint, ParseError> {
        todo!("constraint is not supported yet");
    }
}
