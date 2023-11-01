use bevy::utils::HashMap;

use crate::open_scenario::osc2::runner::lex::{lexer::Spanned, token::Token};

use super::{
    constraint::Constraint,
    errors::{ParseError, ParseErrorType},
    event::Event,
    field::Field,
    identifier::Identifier,
    method::Method,
    parser::SpanIterator,
    value::Value,
};

#[derive(Debug, Default)]
pub(super) struct Struct {
    name: Identifier,
    parent_struct: Option<Identifier>,
    initial_field_values: HashMap<Identifier, Value>,
    member_declaration: Vec<StructMemberDeclaration>,
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum StructMemberDeclaration {
    Event(Event),
    Field(Field),
    Constraint(Constraint),
    Method(Method),
    Coverage,
    #[default]
    None,
}

impl Struct {
    pub fn parse_struct(span_iter: &mut SpanIterator) -> Result<Struct, ParseError> {
        let mut osc_struct = Struct::default();
        osc_struct.name = Struct::parse_struct_name(span_iter)?;
        while let Some(span) = span_iter.peek(0) {
            match span.token {
                Token::Inherits => {
                    return Err(ParseError {
                        error: ParseErrorType::Unsupported {
                            found: span.token.clone(),
                        },
                        token_loc: Some(span.start_loc.clone()),
                    });
                }
                Token::Colon => {
                    span_iter.next(); // consume colon
                    let Some(nxt) = span_iter.next() else {
                    return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None })};
                    if nxt.token != Token::Newline {
                        return Err(ParseError {
                            error: ParseErrorType::UnexpectedToken {
                                found: nxt.token.clone(),
                                expected: vec![Token::Newline],
                            },
                            token_loc: Some(nxt.start_loc.clone()),
                        });
                    }
                    let Some(nxt) = span_iter.next() else {
                    return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None })};
                    if nxt.token != Token::Indent {
                        return Err(ParseError {
                            error: ParseErrorType::UnexpectedToken {
                                found: nxt.token.clone(),
                                expected: vec![Token::Indent],
                            },
                            token_loc: Some(nxt.start_loc.clone()),
                        });
                    }
                    osc_struct.member_declaration = Struct::parse_struct_member(span_iter)?;
                }
                _ => {
                    break;
                } //Token::Newline => {
                  //    break;
                  //}
                  //_ => {
                  //    return Err(ParseError {
                  //        error: ParseErrorType::UnexpectedToken {
                  //            found: span.token.clone(),
                  //            expected: vec![Token::Colon, Token::Newline, Token::Inherits],
                  //        },
                  //        token_loc: Some(span.start_loc.clone()),
                  //    });
                  //}
            }
        }
        Ok(osc_struct)
    }

    fn parse_struct_member(
        span_iter: &mut SpanIterator,
    ) -> Result<Vec<StructMemberDeclaration>, ParseError> {
        let mut results = vec![];
        loop {
            match span_iter.peek(0) {
                Some(Spanned {
                    token: Token::Event,
                    ..
                }) => results.push(StructMemberDeclaration::Event(Event::parse_event(
                    span_iter,
                )?)),
                Some(Spanned {
                    token: Token::Keep, ..
                })
                | Some(Spanned {
                    token: Token::RemoveDefault,
                    ..
                }) => results.push(StructMemberDeclaration::Constraint(
                    Constraint::parse_constraint(span_iter)?,
                )),
                Some(Spanned {
                    token: Token::Def, ..
                }) => results.push(StructMemberDeclaration::Method(Method::parse_method(
                    span_iter,
                )?)),
                Some(Spanned {
                    token: Token::Cover,
                    ..
                })
                | Some(Spanned {
                    token: Token::Record,
                    ..
                }) => results.push(StructMemberDeclaration::Coverage),
                Some(Spanned {
                    token: Token::Identifier { identifier: _ },
                    ..
                }) => {
                    let fields = Field::parse_fields(span_iter)?;
                    for field in fields {
                        results.push(StructMemberDeclaration::Field(field));
                    }
                }
                Some(Spanned {
                    token: Token::Dedent,
                    ..
                }) => {
                    span_iter.next();
                    break;
                }
                Some(Spanned {
                    token: Token::Newline,
                    ..
                }) => {
                    // skip empty line in member decl.
                    span_iter.next();
                }
                Some(spanned) => {
                    return Err(ParseError {
                        error: ParseErrorType::UnexpectedToken {
                            found: spanned.token.clone(),
                            expected: vec![
                                Token::Event,
                                Token::Cover,
                                Token::Keep,
                                Token::RemoveDefault,
                                Token::Def,
                                Token::Record,
                                Token::Dedent,
                                Token::Identifier {
                                    identifier: "".to_string(),
                                },
                            ],
                        },
                        token_loc: Some(spanned.start_loc),
                    });
                }
                None => {
                    return Err(ParseError {
                        error: ParseErrorType::EndOfFile,
                        token_loc: None,
                    })
                }
            }
        }
        Ok(results)
    }

    fn parse_struct_name(span_iter: &mut SpanIterator) -> Result<Identifier, ParseError> {
        if let Some(span) = span_iter.next() {
            match &span.token {
                Token::Identifier { identifier } => Ok(Identifier {
                    name: identifier.to_owned(),
                }),
                _ => Err(ParseError {
                    error: ParseErrorType::UnexpectedToken {
                        found: span.token.clone(),
                        expected: vec![Token::Identifier {
                            identifier: "".to_string(),
                        }],
                    },
                    token_loc: Some(span.start_loc),
                }),
            }
        } else {
            Err(ParseError {
                error: ParseErrorType::EndOfFile,
                token_loc: None,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::open_scenario::osc2::runner::{
        ast::{
            field::{Parameter, Variable},
            parser::Spans,
            types::Type,
        },
        lex::lexer::make_tokenizer,
    };

    pub fn lex_source(source: &str) -> Spans {
        let lexer = make_tokenizer(source, "".to_string());
        Spans::new(lexer.map(|x| x.unwrap()).collect())
    }

    #[test]
    pub fn test_parse_struct() {
        let source = "
struct my_struct:
  data1: int
  data2, data3, data4: bool
  var speed: speed
";
        let spans = lex_source(source);
        let mut span_iter = spans.iter();
        span_iter.next(); // need to consume leading struct token.
        let result = Struct::parse_struct(&mut span_iter).unwrap();
        assert_eq!(
            result.name,
            Identifier {
                name: "my_struct".to_string()
            }
        );
        assert_eq!(result.initial_field_values.len(), 0);
        assert_eq!(result.member_declaration.len(), 5);
        assert_eq!(
            result.member_declaration[0],
            StructMemberDeclaration::Field(Field::Parameter(Parameter {
                name: Identifier {
                    name: "data1".to_string()
                },
                field_type: Type::Int,
                default_value: None,
                constraints: vec![],
            }))
        );
        assert_eq!(
            result.member_declaration[1],
            StructMemberDeclaration::Field(Field::Parameter(Parameter {
                name: Identifier {
                    name: "data2".to_string()
                },
                field_type: Type::Bool,
                default_value: None,
                constraints: vec![],
            }))
        );
        assert_eq!(
            result.member_declaration[2],
            StructMemberDeclaration::Field(Field::Parameter(Parameter {
                name: Identifier {
                    name: "data3".to_string()
                },
                field_type: Type::Bool,
                default_value: None,
                constraints: vec![],
            }))
        );
        assert_eq!(
            result.member_declaration[3],
            StructMemberDeclaration::Field(Field::Parameter(Parameter {
                name: Identifier {
                    name: "data4".to_string()
                },
                field_type: Type::Bool,
                default_value: None,
                constraints: vec![],
            }))
        );
        assert_eq!(
            result.member_declaration[4],
            StructMemberDeclaration::Field(Field::Variable(Variable {
                name: Identifier {
                    name: "speed".to_string()
                },
                field_type: Type::UserDefinedType(Identifier {
                    name: "speed".to_string()
                }),
                default_value: None,
                sample_expression: None,
            }))
        );
    }
}
