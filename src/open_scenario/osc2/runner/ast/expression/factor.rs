use crate::open_scenario::osc2::runner::{
    ast::{
        errors::ParseError,
        expression::{EvaluationError, ExpressionValue},
        parser::SpanIterator,
        utils,
    },
    lex::token::Token,
};

use super::postfix::PostfixExpression;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Factor {
    PostfixExpression(PostfixExpression),
    NegativeFactor(Box<Factor>),
}

impl Factor {
    pub fn parse_factor(span_iter: &mut SpanIterator) -> Result<Factor, ParseError> {
        if utils::match_peek_next(span_iter, Token::Minus) {
            span_iter.next();
            Ok(Factor::NegativeFactor(Box::new(Factor::parse_factor(
                span_iter,
            )?)))
        } else {
            Ok(Factor::PostfixExpression(
                PostfixExpression::parse_post_fix_expression(span_iter)?,
            ))
        }
    }

    pub fn eval(&self) -> Result<ExpressionValue, EvaluationError> {
        use Factor::*;
        match self {
            PostfixExpression(exp) => exp.eval(),
            NegativeFactor(exp) => {
                let res = exp.eval()?;
                match res {
                    ExpressionValue::Float(val) => Ok(ExpressionValue::Float(-val)),
                    ExpressionValue::Int(val) => Ok(ExpressionValue::Int(-val)),
                    _ => Err(EvaluationError::InvalidOperation),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use ordered_float::OrderedFloat;

    use crate::open_scenario::osc2::runner::ast::{
        expression::{primary::PrimaryExpression, value::ValueExpression},
        identifier::Identifier,
        tests::util::lex_source,
    };

    use super::*;

    #[test]
    pub fn test_post_fix_expression() {
        let test_cases = vec![
            (
                "123.12345".to_string(),
                Ok(PostfixExpression::Primary(PrimaryExpression::Value(
                    ValueExpression::Float(OrderedFloat(123.12345)),
                ))),
            ),
            (
                "actor_name.field_name".to_string(),
                Ok(PostfixExpression::FieldAccess {
                    postfix: Box::new(PostfixExpression::Primary(PrimaryExpression::Identifier(
                        Identifier {
                            name: "actor_name".to_string(),
                        },
                    ))),
                    field_name: Identifier {
                        name: "field_name".to_string(),
                    },
                }),
            ),
            ("actor_name. .field_name".to_string(), Err(())),
            (format!(r##""""long string expression"""##), Err(())), // missing one double-quatation
        ];
        for (source, expected) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = PostfixExpression::parse_post_fix_expression(&mut span_iter);
            if expected.is_ok() {
                assert_eq!(result.unwrap(), expected.unwrap());
            } else {
                assert!(result.is_err());
            }
        }
    }
}
