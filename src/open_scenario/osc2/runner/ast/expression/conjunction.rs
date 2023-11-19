use crate::open_scenario::osc2::runner::{
    ast::{errors::ParseError, parser::SpanIterator},
    lex::token::Token,
};

use super::{inversion::Inversion, EvaluationError, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Conjunction {
    pub inversions: Vec<Inversion>,
}

impl Conjunction {
    pub fn parse_conjunction(span_iter: &mut SpanIterator) -> Result<Conjunction, ParseError> {
        let mut conjunction = Conjunction { inversions: vec![] };
        conjunction
            .inversions
            .push(Inversion::parse_inversion(span_iter)?);

        while let Some(span) = span_iter.peek(0) {
            if span.token == Token::And {
                span_iter.next();
                conjunction
                    .inversions
                    .push(Inversion::parse_inversion(span_iter)?);
            } else {
                break;
            }
        }
        Ok(conjunction)
    }

    pub fn eval(&self) -> Result<ExpressionValue, EvaluationError> {
        assert!(!self.inversions.is_empty());
        if self.inversions.len() == 1 {
            return self.inversions[0].eval();
        }

        let mut res = ExpressionValue::Bool(true);
        for inv in self.inversions.iter() {
            match (res, inv.eval()?) {
                (ExpressionValue::Bool(curr), ExpressionValue::Bool(val)) => {
                    res = ExpressionValue::Bool(curr && val);
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
            postfix::PostfixExpression,
            primary::PrimaryExpression,
            relation::{BinaryRelation, BinaryRelationOp, Relation},
            sum::Sum,
            term::Term,
            value::ValueExpression,
        },
        tests::util::lex_source,
    };

    use super::*;

    #[test]
    pub fn test_conjunction() {
        let test_cases: Vec<(
            String,
            Result<Conjunction, ()>,
            Result<ExpressionValue, EvaluationError>,
        )> = vec![(
            "true and not false and 1 == 1".to_string(),
            Ok(Conjunction {
                inversions: vec![
                    Inversion::Relation(Relation {
                        left_sum: Sum {
                            term: Term {
                                factor: Factor::PostfixExpression(PostfixExpression {
                                    primary_expr: PrimaryExpression::Value(ValueExpression::Bool(
                                        true,
                                    )),
                                    inner_exprs: vec![],
                                }),
                                multiplications: vec![],
                            },
                            additivations: vec![],
                        },
                        binary_relation: None,
                    }),
                    Inversion::Not(Relation {
                        left_sum: Sum {
                            term: Term {
                                factor: Factor::PostfixExpression(PostfixExpression {
                                    primary_expr: PrimaryExpression::Value(ValueExpression::Bool(
                                        false,
                                    )),
                                    inner_exprs: vec![],
                                }),
                                multiplications: vec![],
                            },
                            additivations: vec![],
                        },
                        binary_relation: None,
                    }),
                    Inversion::Relation(Relation {
                        left_sum: Sum {
                            term: Term {
                                factor: Factor::PostfixExpression(PostfixExpression {
                                    primary_expr: PrimaryExpression::Value(
                                        ValueExpression::Uinteger(1),
                                    ),
                                    inner_exprs: vec![],
                                }),
                                multiplications: vec![],
                            },
                            additivations: vec![],
                        },
                        binary_relation: Some(BinaryRelation {
                            operator: BinaryRelationOp::Eq,
                            right_sum: Sum {
                                term: Term {
                                    factor: Factor::PostfixExpression(PostfixExpression {
                                        primary_expr: PrimaryExpression::Value(
                                            ValueExpression::Uinteger(1),
                                        ),
                                        inner_exprs: vec![],
                                    }),
                                    multiplications: vec![],
                                },
                                additivations: vec![],
                            },
                        }),
                    }),
                ],
            }),
            Ok(ExpressionValue::Bool(true)),
        )];
        for (source, expected, expected_val) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = Conjunction::parse_conjunction(&mut span_iter);
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
