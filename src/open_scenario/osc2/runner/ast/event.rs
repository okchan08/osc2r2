use super::{errors::ParseError, parser::SpanIterator};

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Event {}

impl Event {
    pub fn parse_event(_span_iter: &mut SpanIterator) -> Result<Event, ParseError> {
        todo!("event is not supported yet")
    }
}
