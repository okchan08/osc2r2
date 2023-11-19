#![allow(dead_code)]
use crate::open_scenario::osc2::runner::{
    ast::{
        argument::ArgumentList,
        errors::{ParseError, ParseErrorType},
        identifier::Identifier,
        parser::SpanIterator,
        utils,
    },
    lex::token::Token,
};

use super::{primary::PrimaryExpression, EvaluationError, Expression, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct PostfixExpression {
    // handle chain of post fix expression
    // ex: my_struct.inner_struct.as(list of int)[0]
    pub primary_expr: PrimaryExpression,
    pub inner_exprs: Vec<InnerPostfixExpression>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum InnerPostfixExpression {
    // TODO cast and type test is not supported yet.
    Cast,
    TypeTest,
    ElementAccess { index: Expression },
    FunctionApplication { args: Option<ArgumentList> },
    FieldAccess { field_name: Identifier },
}

impl PostfixExpression {
    pub fn parse_post_fix_expression(
        span_iter: &mut SpanIterator,
    ) -> Result<PostfixExpression, ParseError> {
        let primary_expr = PrimaryExpression::parse_primary_expression(span_iter)?;
        let mut post_fix_expr = PostfixExpression {
            primary_expr,
            inner_exprs: vec![],
        };

        while let Some(span) = span_iter.peek(0) {
            let inner_expr = match span.token {
                Token::Period => {
                    utils::consume_one_token(span_iter, Token::Period)?;
                    let spnd = span_iter.peek(0).ok_or(ParseError {
                        error: ParseErrorType::EndOfFile,
                        token_loc: None,
                    })?;
                    // TODO fix here to parse inner elements!
                    match &spnd.token {
                        Token::As => todo!("cast is not supported yet"),
                        Token::Is => todo!("type test is not supported yet"),
                        Token::Identifier { identifier } => {
                            span_iter.next();
                            Ok(InnerPostfixExpression::FieldAccess {
                                field_name: Identifier {
                                    name: identifier.to_owned(),
                                },
                            })
                        }
                        _ => Err(ParseError {
                            error: ParseErrorType::UnexpectedToken {
                                found: span.token.clone(),
                                expected: vec![
                                    Token::As,
                                    Token::Is,
                                    Token::Identifier {
                                        identifier: "".to_string(),
                                    },
                                ],
                            },
                            token_loc: Some(span.start_loc),
                        }),
                    }
                }
                Token::Lsqb => todo!("element access is not supported yet"),
                Token::Lpar => todo!("function application is not supported yet"),
                _ => {
                    break;
                }
            }?;
            post_fix_expr.inner_exprs.push(inner_expr);
        }
        Ok(post_fix_expr)
    }

    fn is_primary_expression_start(span_iter: &SpanIterator) -> bool {
        let Some(span) = span_iter.peek(0) else {return false;};
        match span.token {
            Token::It | Token::Lpar | Token::Identifier { identifier: _ } => true,
            // value expression
            Token::IntNumber(_)
            | Token::FloatNumber(_)
            | Token::True
            | Token::False
            | Token::Quotation
            | Token::DoubleQuotation => true,
            _ => false,
        }
    }

    pub fn eval(&self) -> Result<ExpressionValue, EvaluationError> {
        if !self.inner_exprs.is_empty() {
            Err(EvaluationError::NotSupportedYet {
                message: "post fix operation is not supported yet".to_string(),
            })
        } else {
            self.primary_expr.eval()
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::open_scenario::osc2::runner::ast::{
        expression::value::ValueExpression, tests::util::lex_source,
    };

    use super::*;
    #[test]
    pub fn test_primary_expression() {
        let test_cases = vec![
            (
                "123".to_string(),
                Ok(PrimaryExpression::Value(ValueExpression::Uinteger(123))),
            ),
            (
                "-123".to_string(),
                Ok(PrimaryExpression::Value(ValueExpression::Integer(-123))),
            ),
            (
                format!(r##""this is a string expression""##),
                Ok(PrimaryExpression::Value(ValueExpression::String(
                    "this is a string expression".to_string(),
                ))),
            ),
            ("it".to_string(), Ok(PrimaryExpression::It)),
            (
                "hogehoge".to_string(),
                Ok(PrimaryExpression::Identifier(Identifier {
                    name: "hogehoge".to_string(),
                })),
            ),
            // TODO add expression parse test case
            (format!(r##""""long string expression"""##), Err(())), // missing one double-quatation
        ];
        for (source, expected) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = PrimaryExpression::parse_primary_expression(&mut span_iter);
            if expected.is_ok() {
                assert_eq!(result.unwrap(), expected.unwrap());
            } else {
                assert!(result.is_err());
            }
        }
    }

    #[test]
    pub fn test_post_fix_expression() {
        let test_cases: Vec<(String, Result<PostfixExpression, ()>)> = vec![(
            "my_struct.inner_struct.field_name".to_string(),
            Ok(PostfixExpression {
                primary_expr: PrimaryExpression::Identifier(Identifier {
                    name: "my_struct".to_string(),
                }),
                inner_exprs: vec![
                    InnerPostfixExpression::FieldAccess {
                        field_name: Identifier {
                            name: "inner_struct".to_string(),
                        },
                    },
                    InnerPostfixExpression::FieldAccess {
                        field_name: Identifier {
                            name: "field_name".to_string(),
                        },
                    },
                ],
            }),
        )];
        for (source, expected) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let res = PostfixExpression::parse_post_fix_expression(&mut span_iter);
            if expected.is_ok() {
                assert_eq!(res.unwrap(), expected.unwrap());
            } else {
                assert!(res.is_err());
            }
        }
    }
}
