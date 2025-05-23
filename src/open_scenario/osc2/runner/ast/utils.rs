use crate::open_scenario::osc2::runner::lex::{lexer::Spanned, token::Token};

use super::{
    errors::{ParseError, ParseErrorType},
    identifier::Identifier,
    parser::SpanIterator,
};

// parse "actor_name.behavior_name" tokens.
// The first element of the tuple contains actor_name if tokens have actor name,
// the second element contains the behavior name.
pub(super) fn parse_qualified_behavior_name(
    span_iter: &mut SpanIterator,
) -> Result<(Option<Identifier>, Identifier), ParseError> {
    let Some(first_span) = span_iter.next() else {
          return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None});
        };

    let name = match &first_span.token {
        Token::Identifier { identifier } => identifier.to_owned(),
        _ => {
            return Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    found: first_span.token.clone(),
                    expected: vec![Token::Identifier {
                        identifier: "".to_string(),
                    }],
                },
                token_loc: Some(first_span.start_loc),
            });
        }
    };

    if matches!(
        span_iter.peek(0),
        Some(Spanned {
            token: Token::Period,
            ..
        })
    ) {
        // contains actor name.
        span_iter.next();
        let behavior_name = parse_identifier(span_iter)?;
        Ok((Some(Identifier { name }), behavior_name))
    } else {
        Ok((None, Identifier { name }))
    }
}

pub(super) fn parse_identifier(span_iter: &mut SpanIterator) -> Result<Identifier, ParseError> {
    if let Some(span) = span_iter.next() {
        match &span.token {
            Token::Identifier { identifier } => Ok(Identifier {
                name: identifier.to_owned(),
            }),
            _ => Err(ParseError {
                error: ParseErrorType::ActionDeclarationError,
                token_loc: Some(span.start_loc),
            }),
        }
    } else {
        Err(ParseError {
            error: ParseErrorType::EndOfFile,
            token_loc: None,
        })
    }
}

pub(super) fn parse_integer(span_iter: &mut SpanIterator) -> Result<u64, ParseError> {
    if let Some(span) = span_iter.next() {
        match &span.token {
            Token::IntNumber(num) => Ok(*num),
            _ => Err(ParseError {
                error: ParseErrorType::ActionDeclarationError,
                token_loc: Some(span.start_loc),
            }),
        }
    } else {
        Err(ParseError {
            error: ParseErrorType::EndOfFile,
            token_loc: None,
        })
    }
}

pub(super) fn peek_next_is_identifier(span_iter: &SpanIterator) -> bool {
    let Some(span) = span_iter.peek(0) else {
        return false;
    };
    matches!(span.token, Token::Identifier { identifier: _ })
}

pub(super) fn match_peek_next(span_iter: &SpanIterator, expect: Token) -> bool {
    let Some(span) = span_iter.peek(0) else {
        return false;
    };
    span.token == expect
}

pub(super) fn consume_one_token(
    span_iter: &mut SpanIterator,
    expect: Token,
) -> Result<(), ParseError> {
    if let Some(span) = span_iter.next() {
        if span.token == expect {
            Ok(())
        } else {
            Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    found: span.token.clone(),
                    expected: vec![expect],
                },
                token_loc: Some(span.start_loc),
            })
        }
    } else {
        Err(ParseError {
            error: ParseErrorType::EndOfFile,
            token_loc: None,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::tests::util::lex_source;

    use super::*;

    #[test]
    pub fn test_consume_one_token() {
        let source = "
actor with () do";

        let spans = lex_source(source);
        let mut span_iter = spans.iter();
        assert!(consume_one_token(&mut span_iter, Token::Actor).is_ok());
        assert!(consume_one_token(&mut span_iter, Token::With).is_ok());
        assert!(consume_one_token(&mut span_iter, Token::Lpar).is_ok());
        assert!(consume_one_token(&mut span_iter, Token::Rpar).is_ok());
        assert!(consume_one_token(&mut span_iter, Token::Scenario).is_err());
    }
}
