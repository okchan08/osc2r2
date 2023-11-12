use ordered_float::OrderedFloat;

use self::{relation::Relation, sum::Sum};

use super::{errors::ParseError, parser::SpanIterator};

mod factor;
mod postfix;
mod primary;
mod relation;
mod sum;
mod term;
mod value;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expression {
    Implication(Box<Implication>),
    TernaryOp(Box<TernaryOpExpression>),
}

impl Expression {
    pub fn parse_expression(span_iter: &mut SpanIterator) -> Result<Expression, ParseError> {
        todo!()
    }

    pub fn eval(self) -> Result<ExpressionValue, EvaluationError> {
        todo!()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ExpressionValue {
    Float(OrderedFloat<f64>),
    Int(i64),
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
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum EvaluationError {
    InvalidOperation,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct TernaryOpExpression {
    implication: Box<Implication>,
    true_expr: Box<Expression>,
    false_expr: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Implication {
    disjunctions: Vec<Disjunction>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Disjunction {
    conjunctions: Vec<Conjunction>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Conjunction {
    inversions: Vec<Inversion>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Inversion {
    Not(Box<Inversion>),
    Relation(Relation),
}
