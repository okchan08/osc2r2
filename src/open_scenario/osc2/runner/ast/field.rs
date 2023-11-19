use crate::open_scenario::osc2::runner::{
    ast::{errors::ParseErrorType, types::Type},
    lex::{lexer::Spanned, token::Token},
};

use super::{
    constraint::Constraint, errors::ParseError, expression::Expression, identifier::Identifier,
    parser::SpanIterator, utils, value::Value,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum Field {
    Parameter(Parameter),
    Variable(Variable),
}

impl Field {
    // parse field declaration.
    // span_iter is expected to point the beginning of
    // parameter decl(identifier token). or variable decl(Var token).
    pub fn parse_fields(span_iter: &mut SpanIterator) -> Result<Vec<Field>, ParseError> {
        let mut fields = vec![];
        loop {
            match span_iter.peek(0) {
                Some(Spanned {
                    token: Token::Identifier { identifier: _ },
                    ..
                }) => {
                    for param in Parameter::parse_parameters(span_iter)? {
                        fields.push(Field::Parameter(param));
                    }
                }
                Some(Spanned {
                    token: Token::Var, ..
                }) => {
                    for var in Variable::parse_variables(span_iter)? {
                        fields.push(Field::Variable(var));
                    }
                }
                _ => {
                    break;
                }
            }
        }
        Ok(fields)
    }

    fn parse_field_name_list(span_iter: &mut SpanIterator) -> Result<Vec<Identifier>, ParseError> {
        // get list of names from parameter name declaration
        // param1, param2, param3: int  --> result: ["param1", "param2", "param3"]
        // The span iterator points to the type declarator ("int" in the above example) after this method.
        let mut names = vec![];
        loop {
            match span_iter.peek(0) {
                Some(Spanned {
                    token: Token::Identifier { identifier },
                    ..
                }) => {
                    names.push(Identifier {
                        name: identifier.to_owned(),
                    });
                    span_iter.next();
                }
                Some(Spanned {
                    token: Token::Comma,
                    ..
                }) => {
                    span_iter.next();
                }
                Some(Spanned {
                    token: Token::Colon,
                    ..
                }) => {
                    span_iter.next();
                    break;
                }
                _ => {
                    break;
                }
            }
        }
        Ok(names)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct Parameter {
    pub name: Identifier,
    pub field_type: Type,
    pub default_value: Option<Expression>,
    pub constraints: Vec<Constraint>,
}

impl Parameter {
    pub fn parse_parameters(span_iter: &mut SpanIterator) -> Result<Vec<Parameter>, ParseError> {
        let names = Field::parse_field_name_list(span_iter)?;
        let mut constraints = vec![];
        let defined_type = Type::parse_type(span_iter)?;
        let mut default_val = None;
        loop {
            match span_iter.peek(0) {
                Some(Spanned {
                    token: Token::Equal,
                    ..
                }) => {
                    utils::consume_one_token(span_iter, Token::Equal)?;
                    default_val = Some(Expression::parse_expression(span_iter)?);
                }
                Some(Spanned {
                    token: Token::With, ..
                }) => {
                    constraints.push(Constraint::parse_constraint(span_iter)?);
                }
                Some(Spanned {
                    token: Token::Newline,
                    ..
                }) => {
                    span_iter.next();
                    break;
                }
                Some(span) => {
                    return Err(ParseError {
                        error: ParseErrorType::UnexpectedToken {
                            found: span.token.clone(),
                            expected: vec![Token::Equal, Token::Sample, Token::Newline],
                        },
                        token_loc: Some(span.start_loc),
                    });
                }
                None => {
                    return Err(ParseError {
                        error: ParseErrorType::EndOfFile,
                        token_loc: None,
                    });
                }
            }
        }
        Ok(names
            .into_iter()
            .map(|name| Parameter {
                name,
                field_type: defined_type.clone(),
                default_value: default_val.clone(),
                constraints: constraints.to_vec(),
            })
            .collect())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) struct Variable {
    pub name: Identifier,
    pub field_type: Type,
    // both of them are None, or only either one of the two has a value.
    pub default_value: Option<Value>,
    pub sample_expression: Option<Expression>,
}

impl Variable {
    pub fn parse_variables(span_iter: &mut SpanIterator) -> Result<Vec<Variable>, ParseError> {
        // expects span_iter to point Token::Var token on called.
        match span_iter.peek(0) {
            Some(Spanned {
                token: Token::Var, ..
            }) => {
                span_iter.next();
            }
            Some(Spanned {
                token, start_loc, ..
            }) => {
                return Err(ParseError {
                    error: ParseErrorType::UnexpectedToken {
                        found: token.clone(),
                        expected: vec![Token::Var],
                    },
                    token_loc: Some(*start_loc),
                });
            }
            None => {
                return Err(ParseError {
                    error: ParseErrorType::EndOfFile,
                    token_loc: None,
                });
            }
        }
        let names = Field::parse_field_name_list(span_iter)?;
        let defined_type = Type::parse_type(span_iter)?;
        match span_iter.peek(0) {
            Some(Spanned {
                token: Token::Equal,
                start_loc,
                ..
            })
            | Some(Spanned {
                token: Token::Sample,
                start_loc,
                ..
            }) => {
                return Err(ParseError {
                    error: ParseErrorType::Unsupported {
                        found: Token::Equal,
                    },
                    token_loc: Some(*start_loc),
                });
            }
            Some(Spanned {
                token: Token::Newline,
                ..
            }) => {
                span_iter.next();
            }
            Some(span) => {
                return Err(ParseError {
                    error: ParseErrorType::UnexpectedToken {
                        found: span.token.clone(),
                        expected: vec![Token::Equal, Token::Sample, Token::Newline],
                    },
                    token_loc: Some(span.start_loc),
                });
            }
            None => {
                return Err(ParseError {
                    error: ParseErrorType::EndOfFile,
                    token_loc: None,
                });
            }
        }
        Ok(names
            .into_iter()
            .map(|name| Variable {
                name,
                field_type: defined_type.clone(),
                default_value: None,
                sample_expression: None,
            })
            .collect())
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::tests::util::lex_source;

    use super::*;

    #[test]
    pub fn test_field_parser() {
        for (input_str, expected) in vec![
            (
                "param1: int\n",
                vec![Field::Parameter(Parameter {
                    name: Identifier {
                        name: "param1".to_string(),
                    },
                    field_type: Type::Int,
                    default_value: None,
                    constraints: vec![],
                })],
            ),
            (
                "param2, param3: bool\n",
                vec![
                    Field::Parameter(Parameter {
                        name: Identifier {
                            name: "param2".to_string(),
                        },
                        field_type: Type::Bool,
                        default_value: None,
                        constraints: vec![],
                    }),
                    Field::Parameter(Parameter {
                        name: Identifier {
                            name: "param3".to_string(),
                        },
                        field_type: Type::Bool,
                        default_value: None,
                        constraints: vec![],
                    }),
                ],
            ),
            (
                "param4: my_struct\n",
                vec![Field::Parameter(Parameter {
                    name: Identifier {
                        name: "param4".to_string(),
                    },
                    field_type: Type::UserDefinedType(Identifier {
                        name: "my_struct".to_string(),
                    }),
                    default_value: None,
                    constraints: vec![],
                })],
            ),
            (
                "var param5, param6: list of my_struct\n",
                vec![
                    Field::Variable(Variable {
                        name: Identifier {
                            name: "param5".to_string(),
                        },
                        field_type: Type::List(Box::new(Type::UserDefinedType(Identifier {
                            name: "my_struct".to_string(),
                        }))),
                        default_value: None,
                        sample_expression: None,
                    }),
                    Field::Variable(Variable {
                        name: Identifier {
                            name: "param6".to_string(),
                        },
                        field_type: Type::List(Box::new(Type::UserDefinedType(Identifier {
                            name: "my_struct".to_string(),
                        }))),
                        default_value: None,
                        sample_expression: None,
                    }),
                ],
            ),
        ] {
            let spans = lex_source(input_str);
            let result = Field::parse_fields(&mut spans.iter()).unwrap();
            assert_eq!(result.len(), expected.len());
            for (result, expect) in result.iter().zip(expected.iter()) {
                assert_eq!(result, expect);
            }
        }
    }
}
