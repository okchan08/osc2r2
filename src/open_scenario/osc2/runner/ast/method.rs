use crate::open_scenario::osc2::runner::lex::{lexer::Spanned, location::Location, token::Token};

use super::{
    errors::{ParseError, ParseErrorType},
    parser::SpanIterator,
};
#[derive(Default, Debug)]
pub struct Method {}

impl Method {
    pub fn parse_method(span_iter: &mut SpanIterator) -> Result<Method, ParseError> {
        Err(ParseError {
            error: ParseErrorType::Unsupported,
            token_loc: Some(
                span_iter
                    .next()
                    .unwrap_or(&Spanned {
                        start_loc: Location::new(0, 0),
                        end_loc: Location::new(0, 0),
                        token: Token::EndOfFile,
                    })
                    .start_loc,
            ),
        })
    }
}
