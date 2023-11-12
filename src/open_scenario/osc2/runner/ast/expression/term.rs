use ordered_float::OrderedFloat;

use crate::open_scenario::osc2::runner::{
    ast::{
        errors::{ParseError, ParseErrorType},
        expression::{EvaluationError, ExpressionValue},
        parser::SpanIterator,
        utils,
    },
    lex::token::Token,
};

use super::factor::Factor;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum MultiplicativeOperation {
    Mul,
    Div,
    Mod,
}

impl MultiplicativeOperation {
    pub fn parse_operation(
        span_iter: &mut SpanIterator,
    ) -> Result<MultiplicativeOperation, ParseError> {
        let Some(span) = span_iter.next() else {
            return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None });
        };
        match span.token {
            Token::Star => Ok(MultiplicativeOperation::Mul),
            Token::Slash => Ok(MultiplicativeOperation::Div),
            Token::Percent => Ok(MultiplicativeOperation::Mod),
            _ => Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    found: span.token.clone(),
                    expected: vec![Token::Star, Token::Slash, Token::Percent],
                },
                token_loc: Some(span.start_loc.clone()),
            }),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Multiplication {
    pub operation: MultiplicativeOperation,
    pub right_factor: Box<Factor>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Term {
    pub factor: Factor,
    pub multiplications: Vec<Multiplication>,
}

impl Term {
    pub fn parse_term(span_iter: &mut SpanIterator) -> Result<Term, ParseError> {
        let left_factor = Factor::parse_factor(span_iter)?;
        let mut term = Term {
            factor: left_factor,
            multiplications: vec![],
        };
        while let Some(span) = span_iter.peek(0) {
            let result = match span.token {
                Token::Star => {
                    let op = MultiplicativeOperation::Mul;
                    utils::consume_one_token(span_iter, Token::Star)?;
                    let factor = Factor::parse_factor(span_iter)?;
                    Ok((op, factor))
                }
                Token::Slash => {
                    let op = MultiplicativeOperation::Div;
                    utils::consume_one_token(span_iter, Token::Slash)?;
                    let factor = Factor::parse_factor(span_iter)?;
                    Ok((op, factor))
                }
                Token::Percent => {
                    let op = MultiplicativeOperation::Mod;
                    utils::consume_one_token(span_iter, Token::Percent)?;
                    let factor = Factor::parse_factor(span_iter)?;
                    Ok((op, factor))
                }
                _ => Err(()),
            };

            match result {
                Ok((op, factor)) => {
                    term.multiplications.push(Multiplication {
                        operation: op,
                        right_factor: Box::new(factor),
                    });
                }
                Err(_) => {
                    break;
                }
            }
        }
        Ok(term)
    }

    pub fn eval(self) -> Result<ExpressionValue, EvaluationError> {
        use ExpressionValue::*;
        use MultiplicativeOperation::*;
        let mut result = self.factor.eval()?;
        for multiplication in self.multiplications {
            match (result, multiplication.right_factor.eval()?) {
                (Int(left), Int(right)) => match multiplication.operation {
                    Div => {
                        result = ExpressionValue::Int(left / right);
                    }
                    Mod => {
                        result = ExpressionValue::Int(left % right);
                    }
                    Mul => {
                        result = ExpressionValue::Int(left * right);
                    }
                },
                (Int(left), Float(right)) => match multiplication.operation {
                    Div => {
                        result = ExpressionValue::Float(OrderedFloat(left as f64 / right.0));
                    }
                    Mod => {
                        result = ExpressionValue::Float(OrderedFloat(left as f64 % right.0));
                    }
                    Mul => {
                        result = ExpressionValue::Float(OrderedFloat(left as f64 * right.0));
                    }
                },
                (Float(left), Int(right)) => match multiplication.operation {
                    Div => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 / right as f64));
                    }
                    Mod => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 % right as f64));
                    }
                    Mul => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 * right as f64));
                    }
                },
                (Float(left), Float(right)) => match multiplication.operation {
                    Div => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 / right.0));
                    }
                    Mod => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 % right.0));
                    }
                    Mul => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 * right.0));
                    }
                },
                _ => {
                    return Err(EvaluationError::InvalidOperation);
                }
            }
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::{
        expression::{
            postfix::PostfixExpression, primary::PrimaryExpression, value::ValueExpression,
        },
        tests::util::lex_source,
    };

    use super::*;

    #[test]
    pub fn test_term_expression() {
        let test_cases: Vec<(String, Result<Term, ()>, ExpressionValue)> = vec![
            (
                "123".to_string(),
                Ok(Term {
                    factor: Factor::PostfixExpression(PostfixExpression::Primary(
                        PrimaryExpression::Value(ValueExpression::Integer(123)),
                    )),
                    multiplications: vec![],
                }),
                ExpressionValue::Int(123),
            ),
            (
                "-98.7".to_string(),
                Ok(Term {
                    factor: Factor::NegativeFactor(Box::new(Factor::PostfixExpression(
                        PostfixExpression::Primary(PrimaryExpression::Value(
                            ValueExpression::Float(OrderedFloat(98.7)),
                        )),
                    ))),
                    multiplications: vec![],
                }),
                ExpressionValue::Float(OrderedFloat(-98.7)),
            ),
            (
                "-34 * 67.8   ".to_string(),
                Ok(Term {
                    factor: Factor::NegativeFactor(Box::new(Factor::PostfixExpression(
                        PostfixExpression::Primary(PrimaryExpression::Value(
                            ValueExpression::Integer(34),
                        )),
                    ))),
                    multiplications: vec![Multiplication {
                        operation: MultiplicativeOperation::Mul,
                        right_factor: Box::new(Factor::PostfixExpression(
                            PostfixExpression::Primary(PrimaryExpression::Value(
                                ValueExpression::Float(OrderedFloat(67.8)),
                            )),
                        )),
                    }],
                }),
                ExpressionValue::Float(OrderedFloat(-2305.2)),
            ),
            (
                "-34 * 67.8  / 10 ".to_string(),
                Ok(Term {
                    factor: Factor::NegativeFactor(Box::new(Factor::PostfixExpression(
                        PostfixExpression::Primary(PrimaryExpression::Value(
                            ValueExpression::Integer(34),
                        )),
                    ))),
                    multiplications: vec![
                        Multiplication {
                            operation: MultiplicativeOperation::Mul,
                            right_factor: Box::new(Factor::PostfixExpression(
                                PostfixExpression::Primary(PrimaryExpression::Value(
                                    ValueExpression::Float(OrderedFloat(67.8)),
                                )),
                            )),
                        },
                        Multiplication {
                            operation: MultiplicativeOperation::Div,
                            right_factor: Box::new(Factor::PostfixExpression(
                                PostfixExpression::Primary(PrimaryExpression::Value(
                                    ValueExpression::Integer(10),
                                )),
                            )),
                        },
                    ],
                }),
                ExpressionValue::Float(OrderedFloat(-230.51999999999998)), // This is because Rust's cast of i64 to f64
            ),
        ];
        for (source, expected, expected_val) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = Term::parse_term(&mut span_iter);
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
