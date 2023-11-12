use ordered_float::OrderedFloat;

use crate::open_scenario::osc2::runner::{
    ast::{
        errors::{ParseError, ParseErrorType},
        parser::SpanIterator,
        utils,
    },
    lex::token::Token,
};

use super::{EvaluationError, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ValueExpression {
    // TODO enable remaining value expressions.
    Integer(i64),
    Float(OrderedFloat<f64>),
    // Physical
    Bool(bool),
    String(String),
    // Enum
    // List
    // Range
}

impl ValueExpression {
    pub fn parse_value_exp(span_iter: &mut SpanIterator) -> Result<ValueExpression, ParseError> {
        let Some(span) = span_iter.peek(0) else {
          return Err(ParseError{error: ParseErrorType::EndOfFile, token_loc: None});
        };
        match &span.token {
            Token::IntNumber(num) => {
                span_iter.next();
                Ok(ValueExpression::Integer(*num))
            }
            Token::FloatNumber(num) => {
                span_iter.next();
                Ok(ValueExpression::Float(OrderedFloat(*num)))
            }
            Token::True => {
                span_iter.next();
                Ok(ValueExpression::Bool(true))
            }
            Token::False => {
                span_iter.next();
                Ok(ValueExpression::Bool(false))
            }
            Token::DoubleQuotation => {
                span_iter.next();
                if utils::match_peek_next(span_iter, Token::DoubleQuotation) {
                    // long string starting with """
                    utils::consume_one_token(span_iter, Token::DoubleQuotation)?;
                    utils::consume_one_token(span_iter, Token::DoubleQuotation)?;
                    let ret = ValueExpression::concate_to_string(span_iter)?;
                    utils::consume_one_token(span_iter, Token::DoubleQuotation)?;
                    utils::consume_one_token(span_iter, Token::DoubleQuotation)?;
                    utils::consume_one_token(span_iter, Token::DoubleQuotation)?;
                    Ok(ValueExpression::String(ret))
                } else {
                    let ret = ValueExpression::concate_to_string(span_iter)?;
                    utils::consume_one_token(span_iter, Token::DoubleQuotation)?;
                    Ok(ValueExpression::String(ret))
                }
            }
            Token::Quotation => {
                utils::consume_one_token(span_iter, Token::Quotation)?;
                let ret = ValueExpression::concate_to_string(span_iter)?;
                utils::consume_one_token(span_iter, Token::Quotation)?;
                Ok(ValueExpression::String(ret))
            }
            _ => Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    found: span.token.clone(),
                    expected: vec![],
                },
                token_loc: Some(span.start_loc.clone()),
            }),
        }
    }

    fn concate_to_string(span_iter: &mut SpanIterator) -> Result<String, ParseError> {
        let mut ret = String::new();
        let mut count = 0;
        while let Some(candidate_span) = span_iter.peek(0) {
            match &candidate_span.token {
                Token::DoubleQuotation => {
                    break;
                }
                _ => {
                    if count == 0 {
                        ret += format!("{}", candidate_span.token).as_str();
                    } else {
                        ret += format!(" {}", candidate_span.token).as_str();
                    }
                    span_iter.next();
                    count += 1;
                }
            }
        }
        Ok(ret)
    }

    pub fn eval(self) -> Result<ExpressionValue, EvaluationError> {
        match self {
            ValueExpression::Bool(val) => Ok(ExpressionValue::Bool(val)),
            ValueExpression::Integer(val) => Ok(ExpressionValue::Int(val)),
            ValueExpression::Float(val) => Ok(ExpressionValue::Float(val)),
            ValueExpression::String(val) => Ok(ExpressionValue::String(val)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::tests::util::lex_source;

    use super::*;
    #[test]
    pub fn test_value_expression() {
        let test_cases = vec![
            ("123".to_string(), Ok(ValueExpression::Integer(123))),
            (
                "4.567".to_string(),
                Ok(ValueExpression::Float(OrderedFloat(4.567))),
            ),
            ("true".to_string(), Ok(ValueExpression::Bool(true))),
            ("false".to_string(), Ok(ValueExpression::Bool(false))),
            (
                format!(r##""hogehoge""##),
                Ok(ValueExpression::String("hogehoge".to_string())),
            ),
            (
                format!(r##""this is a string expression""##),
                Ok(ValueExpression::String(
                    "this is a string expression".to_string(),
                )),
            ),
            (
                format!(r##""""long string expression""""##),
                Ok(ValueExpression::String(
                    "long string expression".to_string(),
                )),
            ),
            (format!(r##""""long string expression"""##), Err(())), // missing one double-quatation
        ];

        for (source, expected) in test_cases {
            let spans = lex_source(source.as_str());
            let mut span_iter = spans.iter();
            let result = ValueExpression::parse_value_exp(&mut span_iter);
            if expected.is_ok() {
                assert_eq!(result.unwrap(), expected.unwrap());
            } else {
                assert!(result.is_err());
            }
        }
    }
}
