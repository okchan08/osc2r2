use super::{errors::ParseError, parser::SpanIterator};

#[derive(Debug, Default)]
pub(super) enum Constraint {
    #[default]
    Keep,
    RemoveDefault,
}

impl Constraint {
    pub fn parse_constraint(span_iter: &mut SpanIterator) -> Result<Constraint, ParseError> {
        todo!("constraint is not supported yet");
    }
}
