use ordered_float::OrderedFloat;

use crate::open_scenario::osc2::runner::{
    ast::{
        errors::{ParseError, ParseErrorType},
        parser::SpanIterator,
        utils,
    },
    lex::token::Token,
};

use super::{term::Term, EvaluationError, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Additivation {
    operation: AdditiveOp,
    right_term: Box<Term>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Sum {
    pub term: Term,
    pub additivations: Vec<Additivation>,
}

impl Sum {
    pub fn parse_sum(span_iter: &mut SpanIterator) -> Result<Sum, ParseError> {
        let left_term = Term::parse_term(span_iter)?;
        let mut sum = Sum {
            term: left_term,
            additivations: vec![],
        };
        while let Some(span) = span_iter.peek(0) {
            let result = match span.token {
                Token::Plus => {
                    let op = AdditiveOp::Plus;
                    utils::consume_one_token(span_iter, Token::Plus)?;
                    let term = Term::parse_term(span_iter)?;
                    Ok((op, term))
                }
                Token::Minus => {
                    let op = AdditiveOp::Minus;
                    utils::consume_one_token(span_iter, Token::Minus)?;
                    let term = Term::parse_term(span_iter)?;
                    Ok((op, term))
                }
                _ => Err(()),
            };

            match result {
                Ok((op, term)) => sum.additivations.push(Additivation {
                    operation: op,
                    right_term: Box::new(term),
                }),
                Err(_) => {
                    break;
                }
            }
        }
        Ok(sum)
    }

    pub fn eval(self) -> Result<ExpressionValue, EvaluationError> {
        use AdditiveOp::*;
        use ExpressionValue::*;
        let mut result = self.term.eval()?;
        for additivation in self.additivations {
            match (result, additivation.right_term.eval()?) {
                (Int(left), Int(right)) => match additivation.operation {
                    Plus => {
                        result = ExpressionValue::Int(left + right);
                    }
                    Minus => {
                        result = ExpressionValue::Int(left - right);
                    }
                },
                (Int(left), Float(right)) => match additivation.operation {
                    Plus => {
                        result = ExpressionValue::Float(OrderedFloat(left as f64 + right.0));
                    }
                    Minus => {
                        result = ExpressionValue::Float(OrderedFloat(left as f64 - right.0));
                    }
                },
                (Float(left), Int(right)) => match additivation.operation {
                    Plus => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 + right as f64));
                    }
                    Minus => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 - right as f64));
                    }
                },
                (Float(left), Float(right)) => match additivation.operation {
                    Plus => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 + right.0));
                    }
                    Minus => {
                        result = ExpressionValue::Float(OrderedFloat(left.0 - right.0));
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AdditiveOperation {
    left_term: Box<Term>,
    operator: AdditiveOp,
    right_term: Box<Term>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum AdditiveOp {
    Plus,
    Minus,
}

impl AdditiveOp {
    pub fn parse(span_iter: &mut SpanIterator) -> Result<AdditiveOp, ParseError> {
        let Some(span) = span_iter.peek(0) else {
            return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None });
        };
        match &span.token {
            Token::Plus => {
                span_iter.next();
                Ok(AdditiveOp::Plus)
            }
            Token::Minus => {
                span_iter.next();
                Ok(AdditiveOp::Minus)
            }
            _ => Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    found: span.token.clone(),
                    expected: vec![Token::Plus, Token::Minus],
                },
                token_loc: Some(span.start_loc.clone()),
            }),
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
            term::{Multiplication, MultiplicativeOperation},
            value::ValueExpression,
        },
        tests::util::lex_source,
    };

    use super::*;

    #[test]
    pub fn test_sum_expression() {
        let test_cases: Vec<(String, Result<Sum, ()>, ExpressionValue)> = vec![(
            "50.0 + 34 * -67.8  / 10 - 23.0".to_string(),
            Ok(Sum {
                term: Term {
                    factor: Factor::PostfixExpression(PostfixExpression::Primary(
                        PrimaryExpression::Value(ValueExpression::Float(OrderedFloat(50.0))),
                    )),
                    multiplications: vec![],
                },
                additivations: vec![
                    Additivation {
                        operation: AdditiveOp::Plus,
                        right_term: Box::new(Term {
                            factor: Factor::PostfixExpression(PostfixExpression::Primary(
                                PrimaryExpression::Value(ValueExpression::Integer(34)),
                            )),
                            multiplications: vec![
                                Multiplication {
                                    operation: MultiplicativeOperation::Mul,
                                    right_factor: Box::new(Factor::NegativeFactor(Box::new(
                                        Factor::PostfixExpression(PostfixExpression::Primary(
                                            PrimaryExpression::Value(ValueExpression::Float(
                                                OrderedFloat(67.8),
                                            )),
                                        )),
                                    ))),
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
                    },
                    Additivation {
                        operation: AdditiveOp::Minus,
                        right_term: Box::new(Term {
                            factor: Factor::PostfixExpression(PostfixExpression::Primary(
                                PrimaryExpression::Value(ValueExpression::Float(OrderedFloat(
                                    23.0,
                                ))),
                            )),
                            multiplications: vec![],
                        }),
                    },
                ],
            }),
            ExpressionValue::Float(OrderedFloat(-203.51999999999998)), // This is because Rust's cast of i64 to f64
        )];
        for (source, expected, expected_val) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = Sum::parse_sum(&mut span_iter);
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
