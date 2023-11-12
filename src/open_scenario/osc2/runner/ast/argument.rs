use std::collections::BTreeMap;

use crate::open_scenario::osc2::runner::lex::token::Token;

use super::{
    errors::{ParseError, ParseErrorType},
    expression::Expression,
    identifier::Identifier,
    parser::SpanIterator,
    utils,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct ArgumentList {
    positional_args: Vec<Expression>,
    named_args: BTreeMap<Identifier, Expression>,
}

impl ArgumentList {
    pub fn parse_arguments(span_iter: &mut SpanIterator) -> Result<ArgumentList, ParseError> {
        let mut args = ArgumentList {
            positional_args: vec![],
            named_args: BTreeMap::new(),
        };
        let Some(span) = span_iter.peek(0) else {
          return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None });
        };
        match &span.token {
            Token::Identifier { identifier: _ } => {
                // The argument list contains only named arguments
                loop {
                    let (name, expr) = ArgumentList::parse_named_arg(span_iter)?;
                    args.named_args.insert(name, expr);
                    if utils::match_peek_next(span_iter, Token::Comma) {
                        utils::consume_one_token(span_iter, Token::Comma)?;
                    } else {
                        break;
                    }
                }
                Ok(args)
            }
            _ => {
                loop {
                    let expr = Expression::parse_expression(span_iter)?;
                    args.positional_args.push(expr);
                    if utils::match_peek_next(span_iter, Token::Comma) {
                        utils::consume_one_token(span_iter, Token::Comma)?;
                        if utils::peek_next_is_identifier(span_iter) {
                            // stard of the named arguments
                            break;
                        }
                        if span_iter.peek(0).is_none() {
                            return Err(ParseError {
                                error: ParseErrorType::EndOfFile,
                                token_loc: None,
                            });
                        }
                    } else {
                        break;
                    }
                }
                if utils::peek_next_is_identifier(span_iter) {
                    loop {
                        let (name, expr) = ArgumentList::parse_named_arg(span_iter)?;
                        args.named_args.insert(name, expr);
                        if utils::match_peek_next(span_iter, Token::Comma) {
                            utils::consume_one_token(span_iter, Token::Comma)?;
                        } else {
                            break;
                        }
                    }
                }
                Ok(args)
            }
        }
    }

    fn parse_named_arg(
        span_iter: &mut SpanIterator,
    ) -> Result<(Identifier, Expression), ParseError> {
        let name = utils::parse_identifier(span_iter)?;
        utils::consume_one_token(span_iter, Token::Colon)?;
        let expr = Expression::parse_expression(span_iter)?;
        Ok((name, expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::expression::sum::{Additivation, AdditiveOp};
    use crate::open_scenario::osc2::runner::ast::expression::{
        conjunction::Conjunction, disjunction::Disjunction, factor::Factor,
        implication::Implication, inversion::Inversion, postfix::PostfixExpression,
        primary::PrimaryExpression, relation::Relation, sum::Sum, term::Term,
        value::ValueExpression,
    };
    use crate::open_scenario::osc2::runner::ast::tests::util::lex_source;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    pub fn test_argument_list() {
        let test_cases = vec![
            (
                "1, 2, 3 + 4".to_string(),
                Ok(ArgumentList {
                    positional_args: vec![
                        Expression::Implication(Implication {
                            disjunctions: vec![Disjunction {
                                conjunctions: vec![Conjunction {
                                    inversions: vec![Inversion::Relation(Relation {
                                        left_sum: Sum {
                                            term: Term {
                                                factor: Factor::PostfixExpression(
                                                    PostfixExpression::Primary(
                                                        PrimaryExpression::Value(
                                                            ValueExpression::Integer(1),
                                                        ),
                                                    ),
                                                ),
                                                multiplications: vec![],
                                            },
                                            additivations: vec![],
                                        },
                                        binary_relation: None,
                                    })],
                                }],
                            }],
                        }),
                        Expression::Implication(Implication {
                            disjunctions: vec![Disjunction {
                                conjunctions: vec![Conjunction {
                                    inversions: vec![Inversion::Relation(Relation {
                                        left_sum: Sum {
                                            term: Term {
                                                factor: Factor::PostfixExpression(
                                                    PostfixExpression::Primary(
                                                        PrimaryExpression::Value(
                                                            ValueExpression::Integer(2),
                                                        ),
                                                    ),
                                                ),
                                                multiplications: vec![],
                                            },
                                            additivations: vec![],
                                        },
                                        binary_relation: None,
                                    })],
                                }],
                            }],
                        }),
                        Expression::Implication(Implication {
                            disjunctions: vec![Disjunction {
                                conjunctions: vec![Conjunction {
                                    inversions: vec![Inversion::Relation(Relation {
                                        left_sum: Sum {
                                            term: Term {
                                                factor: Factor::PostfixExpression(
                                                    PostfixExpression::Primary(
                                                        PrimaryExpression::Value(
                                                            ValueExpression::Integer(3),
                                                        ),
                                                    ),
                                                ),
                                                multiplications: vec![],
                                            },
                                            additivations: vec![Additivation {
                                                operation: AdditiveOp::Plus,
                                                right_term: Box::new(Term {
                                                    factor: Factor::PostfixExpression(
                                                        PostfixExpression::Primary(
                                                            PrimaryExpression::Value(
                                                                ValueExpression::Integer(4),
                                                            ),
                                                        ),
                                                    ),
                                                    multiplications: vec![],
                                                }),
                                            }],
                                        },
                                        binary_relation: None,
                                    })],
                                }],
                            }],
                        }),
                    ],
                    named_args: BTreeMap::new(),
                }),
            ),
            ("1,".to_string(), Err(())),
            (
                "1, 2, hoge : 3, fuga : 4".to_string(),
                Ok(ArgumentList {
                    positional_args: vec![
                        Expression::Implication(Implication {
                            disjunctions: vec![Disjunction {
                                conjunctions: vec![Conjunction {
                                    inversions: vec![Inversion::Relation(Relation {
                                        left_sum: Sum {
                                            term: Term {
                                                factor: Factor::PostfixExpression(
                                                    PostfixExpression::Primary(
                                                        PrimaryExpression::Value(
                                                            ValueExpression::Integer(1),
                                                        ),
                                                    ),
                                                ),
                                                multiplications: vec![],
                                            },
                                            additivations: vec![],
                                        },
                                        binary_relation: None,
                                    })],
                                }],
                            }],
                        }),
                        Expression::Implication(Implication {
                            disjunctions: vec![Disjunction {
                                conjunctions: vec![Conjunction {
                                    inversions: vec![Inversion::Relation(Relation {
                                        left_sum: Sum {
                                            term: Term {
                                                factor: Factor::PostfixExpression(
                                                    PostfixExpression::Primary(
                                                        PrimaryExpression::Value(
                                                            ValueExpression::Integer(2),
                                                        ),
                                                    ),
                                                ),
                                                multiplications: vec![],
                                            },
                                            additivations: vec![],
                                        },
                                        binary_relation: None,
                                    })],
                                }],
                            }],
                        }),
                    ],
                    named_args: vec![
                        (
                            Identifier {
                                name: "hoge".to_string(),
                            },
                            Expression::Implication(Implication {
                                disjunctions: vec![Disjunction {
                                    conjunctions: vec![Conjunction {
                                        inversions: vec![Inversion::Relation(Relation {
                                            left_sum: Sum {
                                                term: Term {
                                                    factor: Factor::PostfixExpression(
                                                        PostfixExpression::Primary(
                                                            PrimaryExpression::Value(
                                                                ValueExpression::Integer(3),
                                                            ),
                                                        ),
                                                    ),
                                                    multiplications: vec![],
                                                },
                                                additivations: vec![],
                                            },
                                            binary_relation: None,
                                        })],
                                    }],
                                }],
                            }),
                        ),
                        (
                            Identifier {
                                name: "fuga".to_string(),
                            },
                            Expression::Implication(Implication {
                                disjunctions: vec![Disjunction {
                                    conjunctions: vec![Conjunction {
                                        inversions: vec![Inversion::Relation(Relation {
                                            left_sum: Sum {
                                                term: Term {
                                                    factor: Factor::PostfixExpression(
                                                        PostfixExpression::Primary(
                                                            PrimaryExpression::Value(
                                                                ValueExpression::Integer(4),
                                                            ),
                                                        ),
                                                    ),
                                                    multiplications: vec![],
                                                },
                                                additivations: vec![],
                                            },
                                            binary_relation: None,
                                        })],
                                    }],
                                }],
                            }),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                }),
            ),
        ];
        for (source, expected) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = ArgumentList::parse_arguments(&mut span_iter);
            if expected.is_ok() {
                assert_eq!(result.unwrap(), expected.unwrap());
            } else {
                assert!(result.is_err());
            }
        }
    }
}
