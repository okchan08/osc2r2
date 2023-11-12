use crate::open_scenario::osc2::runner::{
    ast::{errors::ParseError, parser::SpanIterator, utils},
    lex::token::Token,
};

use super::{relation::Relation, EvaluationError, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Inversion {
    Not(Relation),
    Relation(Relation),
}

impl Inversion {
    pub fn parse_inversion(span_iter: &mut SpanIterator) -> Result<Inversion, ParseError> {
        if utils::match_peek_next(span_iter, Token::Not) {
            utils::consume_one_token(span_iter, Token::Not)?;
            Ok(Inversion::Not(Relation::parse_relation(span_iter)?))
        } else {
            Ok(Inversion::Relation(Relation::parse_relation(span_iter)?))
        }
    }

    pub fn eval(&self) -> Result<ExpressionValue, EvaluationError> {
        match self {
            Inversion::Not(relation) => {
                let result = relation.eval()?;
                match result {
                    ExpressionValue::Bool(val) => Ok(ExpressionValue::Bool(!val)),
                    _ => Err(EvaluationError::InvalidOperation),
                }
            }
            Inversion::Relation(relation) => {
                let result = relation.eval()?;
                match result {
                    ExpressionValue::Bool(val) => Ok(ExpressionValue::Bool(val)),
                    _ => Err(EvaluationError::InvalidOperation),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::{
        expression::{
            factor::Factor,
            postfix::PostfixExpression,
            primary::PrimaryExpression,
            relation::{BinaryRelation, BinaryRelationOp},
            sum::Sum,
            term::Term,
            value::ValueExpression,
            ExpressionValue,
        },
        tests::util::lex_source,
    };

    use super::*;

    #[test]
    pub fn test_inversion() {
        let test_cases: Vec<(
            String,
            Result<Inversion, ()>,
            Result<ExpressionValue, EvaluationError>,
        )> = vec![
            (
                "not 1 < 2".to_string(),
                Ok(Inversion::Not(Relation {
                    left_sum: Sum {
                        term: Term {
                            factor: Factor::PostfixExpression(PostfixExpression::Primary(
                                PrimaryExpression::Value(ValueExpression::Integer(1)),
                            )),
                            multiplications: vec![],
                        },
                        additivations: vec![],
                    },
                    binary_relation: Some(BinaryRelation {
                        operator: BinaryRelationOp::Lt,
                        right_sum: Sum {
                            term: Term {
                                factor: Factor::PostfixExpression(PostfixExpression::Primary(
                                    PrimaryExpression::Value(ValueExpression::Integer(2)),
                                )),
                                multiplications: vec![],
                            },
                            additivations: vec![],
                        },
                    }),
                })),
                Ok(ExpressionValue::Bool(false)),
            ),
            (
                "not 1".to_string(),
                Ok(Inversion::Not(Relation {
                    left_sum: Sum {
                        term: Term {
                            factor: Factor::PostfixExpression(PostfixExpression::Primary(
                                PrimaryExpression::Value(ValueExpression::Integer(1)),
                            )),
                            multiplications: vec![],
                        },
                        additivations: vec![],
                    },
                    binary_relation: None,
                })),
                Err(EvaluationError::InvalidOperation),
            ),
        ];
        for (source, expected, expected_val) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = Inversion::parse_inversion(&mut span_iter);
            if expected.is_ok() {
                let unwrapped = result.unwrap();
                assert_eq!(unwrapped, expected.unwrap());
                if expected_val.is_ok() {
                    assert_eq!(unwrapped.eval().unwrap(), expected_val.unwrap());
                } else {
                    assert!(unwrapped.eval().is_err());
                }
            } else {
                assert!(result.is_err());
            }
        }
    }
}
