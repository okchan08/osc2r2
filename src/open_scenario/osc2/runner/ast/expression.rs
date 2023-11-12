use ordered_float::OrderedFloat;

use crate::open_scenario::osc2::runner::lex::token::Token;

use super::{
    errors::{ParseError, ParseErrorType},
    identifier::Identifier,
    parser::SpanIterator,
    utils,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum Expression {
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
pub(super) enum ExpressionValue {
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
pub(super) enum EvaluationError {
    InvalidOperation,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct TernaryOpExpression {
    implication: Box<Implication>,
    true_expr: Box<Expression>,
    false_expr: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct Implication {
    disjunctions: Vec<Disjunction>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct Disjunction {
    conjunctions: Vec<Conjunction>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct Conjunction {
    inversions: Vec<Inversion>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum Inversion {
    Not(Box<Inversion>),
    Relation(Relation),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum Relation {
    Sum(Box<Sum>),
    BinaryRelation(BinaryRelation),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct BinaryRelation {
    sum: Box<Sum>,
    operator: BinaryRelationOp,
    right_sum: Box<Sum>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum BinaryRelationOp {
    Eq,    // ==
    NotEq, // !=
    Gt,    // <
    Ge,    // <=
    Lt,    // >
    Le,    // >=
    In,    // in
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct Additivation {
    operation: AdditiveOp,
    right_term: Box<Term>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct Sum {
    term: Term,
    additivations: Vec<Additivation>,
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
pub(super) struct AdditiveOperation {
    left_term: Box<Term>,
    operator: AdditiveOp,
    right_term: Box<Term>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum AdditiveOp {
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum MultiplicativeOperation {
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
pub(super) struct Multiplication {
    operation: MultiplicativeOperation,
    right_factor: Box<Factor>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct Term {
    factor: Factor,
    multiplications: Vec<Multiplication>,
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum Factor {
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

    pub fn eval(self) -> Result<ExpressionValue, EvaluationError> {
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum PostfixExpression {
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

    pub fn eval(self) -> Result<ExpressionValue, EvaluationError> {
        match self {
            PostfixExpression::FieldAccess {
                postfix: _,
                field_name: _,
            } => todo!(),
            PostfixExpression::Primary(exp) => exp.eval(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum PrimaryExpression {
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum ValueExpression {
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
