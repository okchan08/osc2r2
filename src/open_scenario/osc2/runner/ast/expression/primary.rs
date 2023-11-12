use crate::open_scenario::osc2::runner::{
    ast::{
        errors::{ParseError, ParseErrorType},
        identifier::Identifier,
        parser::SpanIterator,
        utils,
    },
    lex::token::Token,
};

use super::{value::ValueExpression, EvaluationError, Expression, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PrimaryExpression {
    Value(ValueExpression),
    It,
    Identifier(Identifier),
    Expression(Box<Expression>),
}

impl PrimaryExpression {
    pub fn parse_primary_expression(
        span_iter: &mut SpanIterator,
    ) -> Result<PrimaryExpression, ParseError> {
        let Some(span) = span_iter.peek(0) else {
            return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None });
        };
        match &span.token {
            Token::Lpar => {
                utils::consume_one_token(span_iter, Token::Lpar)?;
                let expression = Expression::parse_expression(span_iter)?;
                utils::consume_one_token(span_iter, Token::Rpar)?;
                Ok(PrimaryExpression::Expression(Box::new(expression)))
            }
            Token::It => {
                utils::consume_one_token(span_iter, Token::It)?;
                Ok(PrimaryExpression::It)
            }
            Token::Identifier { identifier } => {
                span_iter.next();
                Ok(PrimaryExpression::Identifier(Identifier {
                    name: identifier.to_owned(),
                }))
            }
            _ => {
                let value = ValueExpression::parse_value_exp(span_iter);
                match value {
                    Ok(v) => Ok(PrimaryExpression::Value(v)),
                    Err(e) => Err(e),
                }
            }
        }
    }

    pub fn eval(self) -> Result<ExpressionValue, EvaluationError> {
        match self {
            PrimaryExpression::Value(val) => val.eval(),
            PrimaryExpression::Identifier(_) => todo!(),
            PrimaryExpression::It => todo!(),
            PrimaryExpression::Expression(expression) => expression.eval(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::tests::util::lex_source;

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
