use crate::open_scenario::osc2::runner::{
    ast::{
        errors::{ParseError, ParseErrorType},
        identifier::Identifier,
        parser::SpanIterator,
        utils,
    },
    lex::token::Token,
};

use super::{primary::PrimaryExpression, EvaluationError, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PostfixExpression {
    // TODO define other types.
    Primary(PrimaryExpression),
    FieldAccess {
        postfix: Box<PostfixExpression>,
        field_name: Identifier,
    },
}

impl PostfixExpression {
    pub fn parse_post_fix_expression(
        span_iter: &mut SpanIterator,
    ) -> Result<PostfixExpression, ParseError> {
        let post_fix_exp = if PostfixExpression::is_primary_expression_start(span_iter) {
            Ok(PostfixExpression::Primary(
                PrimaryExpression::parse_primary_expression(span_iter)?,
            ))
        } else {
            PostfixExpression::parse_post_fix_expression(span_iter)
        };

        //let post_fix = PostfixExpression::parse_post_fix_expression(span_iter)?;
        let Some(span) = span_iter.peek(0) else {
            return post_fix_exp;
        };

        match &span.token {
            Token::Period => {
                utils::consume_one_token(span_iter, Token::Period)?;
                let Some(next_span) = span_iter.peek(0) else {
                    return Err(ParseError{error: ParseErrorType::EndOfFile, token_loc: None});
                };
                match &next_span.token {
                    Token::As => {
                        utils::consume_one_token(span_iter, Token::As)?;
                        utils::consume_one_token(span_iter, Token::Lpar)?;
                        todo!("cast is not supported")
                    }
                    Token::Is => {
                        utils::consume_one_token(span_iter, Token::Is)?;
                        utils::consume_one_token(span_iter, Token::Lpar)?;
                        todo!("cast is not supported")
                    }
                    Token::Identifier { identifier } => {
                        span_iter.next();
                        return Ok(PostfixExpression::FieldAccess {
                            postfix: Box::new(post_fix_exp?),
                            field_name: Identifier {
                                name: identifier.to_owned(),
                            },
                        });
                    }
                    _ => {
                        return Err(ParseError {
                            error: ParseErrorType::UnexpectedToken {
                                found: next_span.token.clone(),
                                expected: vec![
                                    Token::As,
                                    Token::Is,
                                    Token::Identifier {
                                        identifier: "".to_string(),
                                    },
                                ],
                            },
                            token_loc: Some(next_span.start_loc.clone()),
                        });
                    }
                }
            }
            Token::Lpar => {
                println!("function application not supported");
                return Err(ParseError {
                    error: ParseErrorType::Unsupported { found: Token::Lpar },
                    token_loc: Some(span.start_loc),
                });
            }
            Token::Lsqb => {
                println!("element access is not supported");
                return Err(ParseError {
                    error: ParseErrorType::Unsupported { found: Token::Lsqb },
                    token_loc: Some(span.start_loc),
                });
            }
            _ => {
                return Ok(post_fix_exp?);
            }
        }
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
        match self {
            PostfixExpression::FieldAccess {
                postfix: _,
                field_name: _,
            } => todo!(),
            PostfixExpression::Primary(exp) => exp.eval(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::{
        expression::value::ValueExpression, tests::util::lex_source,
    };

    use super::*;
    #[test]
    pub fn test_primary_expression() {
        let test_cases = vec![
            (
                "123".to_string(),
                Ok(PrimaryExpression::Value(ValueExpression::Integer(123))),
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
}
