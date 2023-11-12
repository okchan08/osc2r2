use crate::open_scenario::osc2::runner::{
    ast::{errors::ParseError, parser::SpanIterator},
    lex::token::Token,
};

use super::{conjunction::Conjunction, EvaluationError, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Disjunction {
    pub conjunctions: Vec<Conjunction>,
}

impl Disjunction {
    pub fn parse_disjunction(span_iter: &mut SpanIterator) -> Result<Disjunction, ParseError> {
        let mut disjunction = Disjunction {
            conjunctions: vec![],
        };
        disjunction
            .conjunctions
            .push(Conjunction::parse_conjunction(span_iter)?);

        while let Some(span) = span_iter.peek(0) {
            if span.token == Token::Or {
                span_iter.next();
                disjunction
                    .conjunctions
                    .push(Conjunction::parse_conjunction(span_iter)?);
            } else {
                break;
            }
        }
        Ok(disjunction)
    }

    pub fn eval(&self) -> Result<ExpressionValue, EvaluationError> {
        if self.conjunctions.len() == 0 {
            return Ok(ExpressionValue::Bool(true));
        }

        let mut res = ExpressionValue::Bool(true);
        for inv in self.conjunctions.iter() {
            match (res, inv.eval()?) {
                (ExpressionValue::Bool(curr), ExpressionValue::Bool(val)) => {
                    res = ExpressionValue::Bool(curr || val);
                }
                _ => return Err(EvaluationError::InvalidOperation),
            }
        }
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::{
        expression::{
            factor::Factor,
            inversion::Inversion,
            postfix::PostfixExpression,
            primary::PrimaryExpression,
            relation::{BinaryRelation, BinaryRelationOp, Relation},
            sum::{Additivation, AdditiveOp, Sum},
            term::Term,
            value::ValueExpression,
        },
        tests::util::lex_source,
    };

    use super::*;

    #[test]
    pub fn test_disjunction() {
        let test_cases: Vec<(
            String,
            Result<Disjunction, ()>,
            Result<ExpressionValue, EvaluationError>,
        )> = vec![(
            "1 + 1 == 2 or false and true".to_string(),
            Ok(Disjunction {
                conjunctions: vec![
                    Conjunction {
                        inversions: vec![Inversion::Relation(Relation {
                            left_sum: Sum {
                                term: Term {
                                    factor: Factor::PostfixExpression(PostfixExpression::Primary(
                                        PrimaryExpression::Value(ValueExpression::Integer(1)),
                                    )),
                                    multiplications: vec![],
                                },
                                additivations: vec![Additivation {
                                    operation: AdditiveOp::Plus,
                                    right_term: Box::new(Term {
                                        factor: Factor::PostfixExpression(
                                            PostfixExpression::Primary(PrimaryExpression::Value(
                                                ValueExpression::Integer(1),
                                            )),
                                        ),
                                        multiplications: vec![],
                                    }),
                                }],
                            },
                            binary_relation: Some(BinaryRelation {
                                operator: BinaryRelationOp::Eq,
                                right_sum: Sum {
                                    term: Term {
                                        factor: Factor::PostfixExpression(
                                            PostfixExpression::Primary(PrimaryExpression::Value(
                                                ValueExpression::Integer(2),
                                            )),
                                        ),
                                        multiplications: vec![],
                                    },
                                    additivations: vec![],
                                },
                            }),
                        })],
                    },
                    Conjunction {
                        inversions: vec![
                            Inversion::Relation(Relation {
                                left_sum: Sum {
                                    term: Term {
                                        factor: Factor::PostfixExpression(
                                            PostfixExpression::Primary(PrimaryExpression::Value(
                                                ValueExpression::Bool(false),
                                            )),
                                        ),
                                        multiplications: vec![],
                                    },
                                    additivations: vec![],
                                },
                                binary_relation: None,
                            }),
                            Inversion::Relation(Relation {
                                left_sum: Sum {
                                    term: Term {
                                        factor: Factor::PostfixExpression(
                                            PostfixExpression::Primary(PrimaryExpression::Value(
                                                ValueExpression::Bool(true),
                                            )),
                                        ),
                                        multiplications: vec![],
                                    },
                                    additivations: vec![],
                                },
                                binary_relation: None,
                            }),
                        ],
                    },
                ],
            }),
            Ok(ExpressionValue::Bool(true)),
        )];
        for (source, expected, expected_val) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = Disjunction::parse_disjunction(&mut span_iter);
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
