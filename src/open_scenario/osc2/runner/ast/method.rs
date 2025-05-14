use super::{
    errors::{ParseError, ParseErrorType},
    parser::SpanIterator,
};
#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Method {}

impl Method {
    pub fn parse_method(span_iter: &mut SpanIterator) -> Result<Method, ParseError> {
        Err(ParseError {
            error: ParseErrorType::Unsupported {
                found: span_iter.peek(0).unwrap().token.clone(),
            },
            token_loc: Some(span_iter.peek(0).unwrap().start_loc),
        })
    }
}
