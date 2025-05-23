#![allow(dead_code)]
use crate::open_scenario::osc2::runner::{
    ast::{
        errors::{ParseError, ParseErrorType},
        parser::SpanIterator,
        utils,
    },
    lex::token::Token,
};

use super::{sum::Sum, EvaluationError, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Relation {
    pub left_sum: Sum,
    pub binary_relation: Option<BinaryRelation>,
}

impl Relation {
    pub fn parse_relation(span_iter: &mut SpanIterator) -> Result<Relation, ParseError> {
        let left_sum = Sum::parse_sum(span_iter)?;
        if utils::match_peek_next(span_iter, Token::DoubleEq)
            || utils::match_peek_next(span_iter, Token::NotEq)
            || utils::match_peek_next(span_iter, Token::Greater)
            || utils::match_peek_next(span_iter, Token::GreaterEq)
            || utils::match_peek_next(span_iter, Token::Less)
            || utils::match_peek_next(span_iter, Token::LessEq)
            || utils::match_peek_next(span_iter, Token::In)
        {
            let operator = BinaryRelationOp::parse(span_iter)?;
            let right_sum = Sum::parse_sum(span_iter)?;
            Ok(Relation {
                left_sum,
                binary_relation: Some(BinaryRelation {
                    operator,
                    right_sum,
                }),
            })
        } else {
            Ok(Relation {
                left_sum,
                binary_relation: None,
            })
        }
    }

    pub fn eval(&self) -> Result<ExpressionValue, EvaluationError> {
        let left_res = self.left_sum.eval()?;
        match &self.binary_relation {
            Some(bo) => {
                let right_res = bo.right_sum.eval()?;
                bo.operator.eval(left_res, right_res)
            }
            None => Ok(left_res),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct BinaryRelation {
    pub operator: BinaryRelationOp,
    pub right_sum: Sum,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum BinaryRelationOp {
    Eq,    // ==
    NotEq, // !=
    Gt,    // >
    Ge,    // >=
    Lt,    // <
    Le,    // <=
    In,    // in
}

impl BinaryRelationOp {
    pub fn parse(span_iter: &mut SpanIterator) -> Result<BinaryRelationOp, ParseError> {
        use BinaryRelationOp::*;
        let Some(span) = span_iter.next() else {
          return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None });
        };
        match span.token {
            Token::DoubleEq => Ok(Eq),
            Token::NotEq => Ok(NotEq),
            Token::Greater => Ok(Gt),
            Token::GreaterEq => Ok(Ge),
            Token::Less => Ok(Lt),
            Token::LessEq => Ok(Le),
            Token::In => Ok(In),
            _ => Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    found: span.token.clone(),
                    expected: vec![
                        Token::DoubleEq,
                        Token::NotEq,
                        Token::Greater,
                        Token::GreaterEq,
                        Token::Less,
                        Token::LessEq,
                        Token::In,
                    ],
                },
                token_loc: Some(span.start_loc),
            }),
        }
    }

    pub fn eval(
        &self,
        left: ExpressionValue,
        right: ExpressionValue,
    ) -> Result<ExpressionValue, EvaluationError> {
        use ExpressionValue::*;
        let l = match left {
            Int(num) => num as f64,
            Uint(num) => num as f64,
            Float(num) => num.0,
            _ => {
                return Err(EvaluationError::InvalidOperation);
            }
        };
        let r = match right {
            Int(num) => num as f64,
            Uint(num) => num as f64,
            Float(num) => num.0,
            _ => {
                return Err(EvaluationError::InvalidOperation);
            }
        };
        self.eval_internal(l, r)
    }

    fn eval_internal<L, R>(&self, left: L, right: R) -> Result<ExpressionValue, EvaluationError>
    where
        L: PartialOrd<R>,
    {
        use BinaryRelationOp::*;
        match self {
            Eq => Ok(ExpressionValue::Bool(left == right)),
            NotEq => Ok(ExpressionValue::Bool(left != right)),
            Gt => Ok(ExpressionValue::Bool(left > right)),
            Ge => Ok(ExpressionValue::Bool(left >= right)),
            Lt => Ok(ExpressionValue::Bool(left < right)),
            Le => Ok(ExpressionValue::Bool(left <= right)),
            In => Err(EvaluationError::InvalidOperation),
        }
    }
}

#[cfg(test)]
mod tests {
    use ordered_float::OrderedFloat;

    use crate::open_scenario::osc2::runner::ast::{
        expression::{
            factor::Factor,
            postfix::PostfixExpression,
            primary::PrimaryExpression,
            sum::{Additivation, AdditiveOp},
            term::{Multiplication, MultiplicativeOperation, Term},
            value::ValueExpression,
            ExpressionValue,
        },
        tests::util::lex_source,
    };

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    pub fn test_relation() {
        let test_cases: Vec<(String, Result<Relation, ()>, ExpressionValue)> = vec![
            (
                "10 < 20".to_string(),
                Ok(Relation {
                    left_sum: Sum {
                        term: Term {
                            factor: Factor::PostfixExpression(PostfixExpression {
                                primary_expr: PrimaryExpression::Value(ValueExpression::Uinteger(
                                    10,
                                )),
                                inner_exprs: vec![],
                            }),
                            multiplications: vec![],
                        },
                        additivations: vec![],
                    },
                    binary_relation: Some(BinaryRelation {
                        operator: BinaryRelationOp::Lt,
                        right_sum: Sum {
                            term: Term {
                                factor: Factor::PostfixExpression(PostfixExpression {
                                    primary_expr: PrimaryExpression::Value(
                                        ValueExpression::Uinteger(20),
                                    ),
                                    inner_exprs: vec![],
                                }),
                                multiplications: vec![],
                            },
                            additivations: vec![],
                        },
                    }),
                }),
                ExpressionValue::Bool(true),
            ),
            (
                "-123.4".to_string(),
                Ok(Relation {
                    left_sum: Sum {
                        term: Term {
                            factor: Factor::NegativeFactor(Box::new(Factor::PostfixExpression(
                                PostfixExpression {
                                    primary_expr: PrimaryExpression::Value(ValueExpression::Float(
                                        OrderedFloat(123.4),
                                    )),
                                    inner_exprs: vec![],
                                },
                            ))),
                            multiplications: vec![],
                        },
                        additivations: vec![],
                    },
                    binary_relation: None,
                }),
                ExpressionValue::Float(OrderedFloat(-123.4)),
            ),
            (
                "50.0 + 34 * -67.8  / 10 != 23.0 * 2.0 / 2 + 4.0".to_string(),
                Ok(Relation {
                    left_sum: Sum {
                        term: Term {
                            factor: Factor::PostfixExpression(PostfixExpression {
                                primary_expr: PrimaryExpression::Value(ValueExpression::Float(
                                    OrderedFloat(50.0),
                                )),
                                inner_exprs: vec![],
                            }),
                            multiplications: vec![],
                        },
                        additivations: vec![Additivation {
                            operation: AdditiveOp::Plus,
                            right_term: Box::new(Term {
                                factor: Factor::PostfixExpression(PostfixExpression {
                                    primary_expr: PrimaryExpression::Value(
                                        ValueExpression::Uinteger(34),
                                    ),
                                    inner_exprs: vec![],
                                }),
                                multiplications: vec![
                                    Multiplication {
                                        operation: MultiplicativeOperation::Mul,
                                        right_factor: Box::new(Factor::NegativeFactor(Box::new(
                                            Factor::PostfixExpression(PostfixExpression {
                                                primary_expr: PrimaryExpression::Value(
                                                    ValueExpression::Float(OrderedFloat(67.8)),
                                                ),
                                                inner_exprs: vec![],
                                            }),
                                        ))),
                                    },
                                    Multiplication {
                                        operation: MultiplicativeOperation::Div,
                                        right_factor: Box::new(Factor::PostfixExpression(
                                            PostfixExpression {
                                                primary_expr: PrimaryExpression::Value(
                                                    ValueExpression::Uinteger(10),
                                                ),
                                                inner_exprs: vec![],
                                            },
                                        )),
                                    },
                                ],
                            }),
                        }],
                    },
                    binary_relation: Some(BinaryRelation {
                        operator: BinaryRelationOp::NotEq,
                        // 23.0 * 2.0 / 2 + 4.0
                        right_sum: Sum {
                            term: Term {
                                factor: Factor::PostfixExpression(PostfixExpression {
                                    primary_expr: PrimaryExpression::Value(ValueExpression::Float(
                                        OrderedFloat(23.0),
                                    )),
                                    inner_exprs: vec![],
                                }),
                                multiplications: vec![
                                    Multiplication {
                                        operation: MultiplicativeOperation::Mul,
                                        right_factor: Box::new(Factor::PostfixExpression(
                                            PostfixExpression {
                                                primary_expr: PrimaryExpression::Value(
                                                    ValueExpression::Float(OrderedFloat(2.0)),
                                                ),
                                                inner_exprs: vec![],
                                            },
                                        )),
                                    },
                                    Multiplication {
                                        operation: MultiplicativeOperation::Div,
                                        right_factor: Box::new(Factor::PostfixExpression(
                                            PostfixExpression {
                                                primary_expr: PrimaryExpression::Value(
                                                    ValueExpression::Uinteger(2),
                                                ),
                                                inner_exprs: vec![],
                                            },
                                        )),
                                    },
                                ],
                            },
                            additivations: vec![Additivation {
                                operation: AdditiveOp::Plus,
                                right_term: Box::new(Term {
                                    factor: Factor::PostfixExpression(PostfixExpression {
                                        primary_expr: PrimaryExpression::Value(
                                            ValueExpression::Float(OrderedFloat(4.0)),
                                        ),
                                        inner_exprs: vec![],
                                    }),
                                    multiplications: vec![],
                                }),
                            }],
                        },
                    }),
                }),
                ExpressionValue::Bool(true),
            ),
            (
                "1 + 2 - 3 + 4 == 5 + 6 - 7".to_string(),
                Ok(Relation {
                    left_sum: Sum {
                        term: Term {
                            factor: Factor::PostfixExpression(PostfixExpression {
                                primary_expr: PrimaryExpression::Value(ValueExpression::Uinteger(
                                    1,
                                )),
                                inner_exprs: vec![],
                            }),
                            multiplications: vec![],
                        },
                        additivations: vec![
                            Additivation {
                                operation: AdditiveOp::Plus,
                                right_term: Box::new(Term {
                                    factor: Factor::PostfixExpression(PostfixExpression {
                                        primary_expr: PrimaryExpression::Value(
                                            ValueExpression::Uinteger(2),
                                        ),
                                        inner_exprs: vec![],
                                    }),
                                    multiplications: vec![],
                                }),
                            },
                            Additivation {
                                operation: AdditiveOp::Minus,
                                right_term: Box::new(Term {
                                    factor: Factor::PostfixExpression(PostfixExpression {
                                        primary_expr: PrimaryExpression::Value(
                                            ValueExpression::Uinteger(3),
                                        ),
                                        inner_exprs: vec![],
                                    }),
                                    multiplications: vec![],
                                }),
                            },
                            Additivation {
                                operation: AdditiveOp::Plus,
                                right_term: Box::new(Term {
                                    factor: Factor::PostfixExpression(PostfixExpression {
                                        primary_expr: PrimaryExpression::Value(
                                            ValueExpression::Uinteger(4),
                                        ),
                                        inner_exprs: vec![],
                                    }),
                                    multiplications: vec![],
                                }),
                            },
                        ],
                    },
                    binary_relation: Some(BinaryRelation {
                        operator: BinaryRelationOp::Eq,
                        right_sum: Sum {
                            term: Term {
                                factor: Factor::PostfixExpression(PostfixExpression {
                                    primary_expr: PrimaryExpression::Value(
                                        ValueExpression::Uinteger(5),
                                    ),
                                    inner_exprs: vec![],
                                }),
                                multiplications: vec![],
                            },
                            additivations: vec![
                                Additivation {
                                    operation: AdditiveOp::Plus,
                                    right_term: Box::new(Term {
                                        factor: Factor::PostfixExpression(PostfixExpression {
                                            primary_expr: PrimaryExpression::Value(
                                                ValueExpression::Uinteger(6),
                                            ),
                                            inner_exprs: vec![],
                                        }),
                                        multiplications: vec![],
                                    }),
                                },
                                Additivation {
                                    operation: AdditiveOp::Minus,
                                    right_term: Box::new(Term {
                                        factor: Factor::PostfixExpression(PostfixExpression {
                                            primary_expr: PrimaryExpression::Value(
                                                ValueExpression::Uinteger(7),
                                            ),
                                            inner_exprs: vec![],
                                        }),
                                        multiplications: vec![],
                                    }),
                                },
                            ],
                        },
                    }),
                }),
                ExpressionValue::Bool(true),
            ),
        ];
        for (source, expected, expected_val) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = Relation::parse_relation(&mut span_iter);
            if expected.is_ok() {
                let unwrapped = result.unwrap();
                assert_eq!(unwrapped, expected.unwrap());
                assert_eq!(unwrapped.eval().unwrap(), expected_val);
            } else {
                assert!(result.is_err());
            }
        }
    }
}
