#![allow(dead_code)]
use crate::open_scenario::osc2::runner::{
    ast::{errors::ParseError, parser::SpanIterator},
    lex::token::Token,
};

use super::{disjunction::Disjunction, EvaluationError, ExpressionValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Implication {
    pub disjunctions: Vec<Disjunction>,
}

impl Implication {
    pub fn parse_implication(span_iter: &mut SpanIterator) -> Result<Implication, ParseError> {
        let mut implication = Implication {
            disjunctions: vec![Disjunction::parse_disjunction(span_iter)?],
        };

        while let Some(span) = span_iter.peek(0) {
            if span.token == Token::REqArrow {
                span_iter.next();
                implication
                    .disjunctions
                    .push(Disjunction::parse_disjunction(span_iter)?);
            } else {
                break;
            }
        }

        Ok(implication)
    }

    pub fn eval(&self) -> Result<ExpressionValue, EvaluationError> {
        assert!(!self.disjunctions.is_empty());
        if self.disjunctions.len() == 1 {
            return self.disjunctions[0].eval();
        }

        let mut curr = self.disjunctions[0].eval()?;
        for dis in self.disjunctions.iter().skip(1) {
            match (curr, dis.eval()?) {
                (ExpressionValue::Bool(c), ExpressionValue::Bool(n)) => {
                    curr = ExpressionValue::Bool(!c || n);
                }
                _ => {
                    return Err(EvaluationError::InvalidOperation);
                }
            }
        }
        Ok(curr)
    }
}
