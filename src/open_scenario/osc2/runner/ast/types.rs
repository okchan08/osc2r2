use crate::open_scenario::osc2::runner::lex::{lexer::Spanned, token::Token};

use super::{
    errors::{ParseError, ParseErrorType},
    identifier::Identifier,
    parser::SpanIterator,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum Type {
    Int,
    Uint,
    Float,
    Bool,
    String,
    // We cannot determin which one of physical type/enum/struct/actor
    // in token sequence parser. Convert the identifier token to UserDefinedType
    // and determine the type in later analysis.
    UserDefinedType(Identifier),
    QualifiedBehavior {
        actor: Option<Identifier>,
        behavior: Identifier,
    },

    // List cannot be nested.
    List(Box<Type>),
}

impl Type {
    pub fn parse_type(span_iter: &mut SpanIterator) -> Result<Type, ParseError> {
        match span_iter.peek(0) {
            Some(Spanned {
                token: Token::List, ..
            }) => {
                span_iter.next();
                let Some(of_token_span) = span_iter.next() else {
                  return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None });
                };
                if of_token_span.token != Token::Of {
                    return Err(ParseError {
                        error: ParseErrorType::UnexpectedToken {
                            found: of_token_span.token.clone(),
                            expected: vec![Token::Of],
                        },
                        token_loc: Some(of_token_span.start_loc),
                    });
                }
                Ok(Type::List(Box::new(Type::parse_non_aggregate_type(
                    span_iter,
                )?)))
            }
            _ => Type::parse_non_aggregate_type(span_iter),
        }
    }

    fn parse_non_aggregate_type(span_iter: &mut SpanIterator) -> Result<Type, ParseError> {
        match span_iter.next() {
            Some(Spanned {
                token: Token::Int, ..
            }) => Ok(Type::Int),
            Some(Spanned {
                token: Token::Uint, ..
            }) => Ok(Type::Uint),
            Some(Spanned {
                token: Token::Float,
                ..
            }) => Ok(Type::Float),
            Some(Spanned {
                token: Token::Bool, ..
            }) => Ok(Type::Bool),
            Some(Spanned {
                token: Token::String,
                ..
            }) => Ok(Type::String),
            Some(Spanned {
                token: Token::Identifier { identifier },
                ..
            }) => {
                let first_identifier = identifier.to_owned();
                match span_iter.peek(0) {
                    Some(Spanned {
                        token: Token::Period,
                        ..
                    }) => {
                        // something like "my_actor.my_behavior" input.
                        span_iter.next();
                        match span_iter.next() {
                            Some(Spanned {
                                token: Token::Identifier { identifier },
                                ..
                            }) => Ok(Type::QualifiedBehavior {
                                actor: Some(Identifier {
                                    name: first_identifier,
                                }),
                                behavior: Identifier {
                                    name: identifier.to_owned(),
                                },
                            }),
                            Some(spanned) => Err(ParseError {
                                error: ParseErrorType::UnexpectedToken {
                                    found: spanned.token.clone(),
                                    expected: vec![Token::Identifier {
                                        identifier: "".to_string(),
                                    }],
                                },
                                token_loc: Some(spanned.start_loc),
                            }),

                            None => Err(ParseError {
                                error: ParseErrorType::EndOfFile,
                                token_loc: None,
                            }),
                        }
                    }
                    _ => Ok(Type::UserDefinedType(Identifier {
                        name: identifier.to_owned(),
                    })),
                }
            }
            Some(spanned) => Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    found: spanned.token.clone(),
                    expected: vec![
                        Token::Int,
                        Token::Uint,
                        Token::Bool,
                        Token::Float,
                        Token::String,
                    ],
                },
                token_loc: Some(spanned.start_loc),
            }),
            None => Err(ParseError {
                error: ParseErrorType::EndOfFile,
                token_loc: None,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::{ast::parser::Spans, lex::location::Location};

    use super::*;

    #[test]
    fn test_primitive_type_parse() {
        for (token, expected) in vec![
            (Token::Int, Type::Int),
            (Token::Uint, Type::Uint),
            (Token::Float, Type::Float),
            (Token::Bool, Type::Bool),
            (Token::String, Type::String),
        ] {
            let spans = Spans::new(vec![Spanned {
                token: token,
                start_loc: Location::new(0, 0),
                end_loc: Location::new(0, 0),
            }]);
            assert_eq!(Type::parse_type(&mut spans.iter()).unwrap(), expected);
        }
    }

    #[test]
    fn test_qualified_type() {
        let loc = Location::new(0, 0);
        let spans = Spans::new(vec![
            Spanned {
                token: Token::Identifier {
                    identifier: "my_actor".to_string(),
                },
                start_loc: loc,
                end_loc: loc,
            },
            Spanned {
                token: Token::Period,
                start_loc: loc,
                end_loc: loc,
            },
            Spanned {
                token: Token::Identifier {
                    identifier: "my_behavior".to_string(),
                },
                start_loc: loc,
                end_loc: loc,
            },
        ]);
        assert_eq!(
            Type::parse_type(&mut spans.iter()).unwrap(),
            Type::QualifiedBehavior {
                actor: Some(Identifier {
                    name: "my_actor".to_string()
                }),
                behavior: Identifier {
                    name: "my_behavior".to_string()
                }
            }
        );
    }

    #[test]
    fn test_list_parse() {
        for (token, expected) in vec![
            (Token::Int, Type::Int),
            (Token::Uint, Type::Uint),
            (Token::Float, Type::Float),
            (Token::Bool, Type::Bool),
            (Token::String, Type::String),
            (
                Token::Identifier {
                    identifier: "my_type".to_string(),
                },
                Type::UserDefinedType(Identifier {
                    name: "my_type".to_string(),
                }),
            ),
        ] {
            let loc = Location::new(0, 0);
            let spans = Spans::new(vec![
                Spanned {
                    token: Token::List,
                    start_loc: loc,
                    end_loc: loc,
                },
                Spanned {
                    token: Token::Of,
                    start_loc: loc,
                    end_loc: loc,
                },
                Spanned {
                    token: token,
                    start_loc: loc,
                    end_loc: loc,
                },
            ]);
            assert_eq!(
                Type::parse_type(&mut spans.iter()).unwrap(),
                Type::List(Box::new(expected))
            );
        }

        {
            // error case
            let loc = Location::new(0, 0);
            let spans = Spans::new(vec![
                Spanned {
                    token: Token::List,
                    start_loc: loc,
                    end_loc: loc,
                },
                Spanned {
                    token: Token::Int,
                    start_loc: loc,
                    end_loc: loc,
                },
            ]);
            assert!(Type::parse_type(&mut spans.iter()).is_err());
        }
    }
}
