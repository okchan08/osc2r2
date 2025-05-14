#![allow(dead_code)]
use crate::open_scenario::osc2::runner::{
    ast::{errors::ParseErrorType, utils},
    lex::token::Token,
};

use super::{errors::ParseError, expression::Expression, parser::SpanIterator};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum Constraint {
    Keep(KeepConstraint),
    RemoveDefault,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub(super) enum ConstraintQualifier {
    Default,
    #[default]
    Hard,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct KeepConstraint {
    constraint_qualifier: ConstraintQualifier,
    expression: Expression,
}

impl Constraint {
    pub fn parse_constraint(span_iter: &mut SpanIterator) -> Result<Constraint, ParseError> {
        if let Some(span) = span_iter.peek(0) {
            match span.token {
                Token::Keep => {
                    return Constraint::parse_keep_constraint(span_iter);
                }
                Token::RemoveDefault => {
                    utils::consume_one_token(span_iter, Token::RemoveDefault)?;
                    return Err(ParseError {
                        error: ParseErrorType::Unsupported {
                            found: Token::RemoveDefault,
                        },
                        token_loc: Some(span.start_loc),
                    });
                }
                _ => {
                    return Err(ParseError {
                        error: ParseErrorType::UnexpectedToken {
                            found: span.token.clone(),
                            expected: vec![Token::Keep, Token::RemoveDefault],
                        },
                        token_loc: Some(span.start_loc),
                    });
                }
            }
        }
        todo!("constraint is not supported yet");
    }

    fn parse_keep_constraint(span_iter: &mut SpanIterator) -> Result<Constraint, ParseError> {
        utils::consume_one_token(span_iter, Token::Keep)?;
        utils::consume_one_token(span_iter, Token::Lpar)?;
        let constraint_qualifier = if utils::match_peek_next(span_iter, Token::Default) {
            ConstraintQualifier::Default
        } else {
            ConstraintQualifier::Hard
        };
        let expression = Expression::parse_expression(span_iter)?;
        utils::consume_one_token(span_iter, Token::Rpar)?;
        utils::consume_one_token(span_iter, Token::Newline)?;
        Ok(Constraint::Keep(KeepConstraint {
            constraint_qualifier,
            expression,
        }))
    }
}
