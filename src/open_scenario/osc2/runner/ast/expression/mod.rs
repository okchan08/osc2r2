use ordered_float::OrderedFloat;

use crate::open_scenario::osc2::runner::lex::token::Token;

use self::implication::Implication;

use super::{errors::ParseError, parser::SpanIterator, utils};

pub(super) mod conjunction;
pub(super) mod disjunction;
pub(super) mod factor;
pub(super) mod implication;
pub(super) mod inversion;
pub(super) mod postfix;
pub(super) mod primary;
pub(super) mod relation;
pub(super) mod sum;
pub(super) mod term;
pub(super) mod value;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expression {
    Implication(Implication),
    TernaryOp {
        implication: Implication,
        true_expr: Box<Expression>,
        false_expr: Box<Expression>,
    },
}

impl Expression {
    pub fn parse_expression(span_iter: &mut SpanIterator) -> Result<Expression, ParseError> {
        let implication = Implication::parse_implication(span_iter)?;
        if utils::match_peek_next(span_iter, Token::Question) {
            utils::consume_one_token(span_iter, Token::Question)?;
            let true_expr = Expression::parse_expression(span_iter)?;
            utils::consume_one_token(span_iter, Token::Colon)?;
            let false_expr = Expression::parse_expression(span_iter)?;
            Ok(Expression::TernaryOp {
                implication: implication,
                true_expr: Box::new(true_expr),
                false_expr: Box::new(false_expr),
            })
        } else {
            Ok(Expression::Implication(implication))
        }
    }

    pub fn eval(&self) -> Result<ExpressionValue, EvaluationError> {
        match self {
            Expression::Implication(implication) => implication.eval(),
            Expression::TernaryOp {
                implication,
                true_expr,
                false_expr,
            } => match implication.eval()? {
                ExpressionValue::Bool(val) => {
                    if val {
                        true_expr.eval()
                    } else {
                        false_expr.eval()
                    }
                }
                _ => Err(EvaluationError::InvalidOperation),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ExpressionValue {
    Float(OrderedFloat<f64>),
    Int(i64),
    Uint(u64),
    String(String),
    Bool(bool),
}

impl ExpressionValue {
    pub fn is_numeric(&self) -> bool {
        use ExpressionValue::*;
        match self {
            Float(_) | Int(_) => true,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        match self {
            ExpressionValue::Bool(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum EvaluationError {
    InvalidOperation,
    NotSupportedYet { message: String },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_expression() {
        //let test_cases = vec![];
    }
}
