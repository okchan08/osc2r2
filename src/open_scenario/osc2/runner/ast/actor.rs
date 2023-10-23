#![allow(dead_code, unused_assignments)]
use std::collections::HashMap;

use crate::open_scenario::osc2::runner::lex::lexer::Spanned;
use crate::open_scenario::osc2::runner::lex::token::Token;

use super::constraint::Constraint;
use super::errors::{ParseError, ParseErrorType};
use super::event::Event;
use super::identifier::Identifier;
use super::method::Method;
use super::parser::SpanIterator;
use super::value::Value;

#[derive(Debug, Default)]
pub(super) struct Actor {
    name: Identifier,
    parent_actor: Option<Identifier>,
    initial_field_values: HashMap<Identifier, Value>,
    member_declarations: Vec<ActorMemberDeclaration>,
}

struct ActorInheritanceDeclaration {
    parent_actor_name: Identifier,
    initial_field_values: HashMap<Identifier, Value>,
}

#[derive(Debug, Default)]
enum ActorMemberDeclaration {
    Event(Event),
    #[default]
    Field,
    Constraint(Constraint),
    Method(Method),
    Coverage,
}

impl Actor {
    pub fn parse_actor_declaration(span_iter: &mut SpanIterator) -> Result<Actor, ParseError> {
        let mut actor = Actor::default();

        let actor_name = Self::parse_actor_name(span_iter)?;
        actor.name = actor_name;
        if let Some(actor_inheritance_decl) = Self::parse_actor_inheritance(span_iter)? {
            actor.parent_actor = Some(actor_inheritance_decl.parent_actor_name);
            actor.initial_field_values = actor_inheritance_decl.initial_field_values;
        }

        if let Some(span) = span_iter.peek(0) {
            match span.token {
                Token::Newline => {
                    // consume empty line.
                    span_iter.next();
                }
                Token::Colon => {
                    // member declarations found.
                    // we expect colon + newline + indent before the actural declarations.
                    span_iter.next(); // consume colon
                    let mut curr_loc = span.start_loc;
                    if let Some(span) = span_iter.next() {
                        // expect newline token
                        curr_loc = span.start_loc;
                        if span.token != Token::Newline {
                            return Err(ParseError {
                                error: ParseErrorType::UnexpectedToken {
                                    found: span.token.clone(),
                                    expected: vec![Token::Newline],
                                },
                                token_loc: Some(span.start_loc),
                            });
                        }
                    } else {
                        return Err(ParseError {
                            error: ParseErrorType::UnexpectedToken {
                                found: Token::EndOfFile,
                                expected: vec![Token::Newline],
                            },
                            token_loc: Some(curr_loc),
                        });
                    }
                    if let Some(span) = span_iter.next() {
                        // expect indent token
                        curr_loc = span.start_loc;
                        if span.token != Token::Indent {
                            return Err(ParseError {
                                error: ParseErrorType::UnexpectedToken {
                                    found: span.token.clone(),
                                    expected: vec![Token::Indent],
                                },
                                token_loc: Some(span.start_loc),
                            });
                        }
                    } else {
                        return Err(ParseError {
                            error: ParseErrorType::UnexpectedToken {
                                found: Token::EndOfFile,
                                expected: vec![Token::Indent],
                            },
                            token_loc: Some(curr_loc),
                        });
                    }
                    actor.member_declarations = Actor::parse_actor_member_declarations(span_iter)?;
                }
                _ => {
                    return Err(ParseError {
                        error: ParseErrorType::UnexpectedToken {
                            found: span.token.clone(),
                            expected: vec![Token::Newline, Token::Colon],
                        },
                        token_loc: Some(span.start_loc),
                    });
                }
            }
        }

        Ok(actor)
    }

    fn parse_actor_name(span_iter: &mut SpanIterator) -> Result<Identifier, ParseError> {
        if let Some(span) = span_iter.next() {
            match &span.token {
                Token::Identifier { identifier } => Ok(Identifier {
                    name: identifier.clone(),
                }),
                _ => Err(ParseError {
                    error: ParseErrorType::ActorDeclarationError,
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

    fn parse_actor_inheritance(
        span_iter: &mut SpanIterator,
    ) -> Result<Option<ActorInheritanceDeclaration>, ParseError> {
        if let Some(span) = span_iter.peek(0) {
            match span.token {
                Token::Inherits => {
                    span_iter.next(); // consume "intehit" token.
                    if let Some(parent_actor_name_span) = span_iter.next() {
                        let _parent_actor_name = match parent_actor_name_span {
                            Spanned {
                                token: Token::Identifier { identifier },
                                ..
                            } => Ok(Identifier {
                                name: identifier.to_string(),
                            }),
                            _ => Err(ParseError {
                                error: ParseErrorType::UnexpectedToken {
                                    found: parent_actor_name_span.token.clone(),
                                    expected: vec![Token::Identifier {
                                        identifier: "".to_string(),
                                    }],
                                },
                                token_loc: Some(parent_actor_name_span.start_loc),
                            }),
                        }?;
                        todo!("field initialization in actor inheritance is not supported yet");
                        // TODO need to parse field initialization here.
                        // Ok(Some(ActorInheritanceDeclaration {
                        //     _parent_actor_name,
                        //     initial_field_values: HashMap::new(),
                        // }))
                    } else {
                        Err(ParseError {
                            error: ParseErrorType::EndOfFile,
                            token_loc: None,
                        })
                    }
                }
                // no inheritance
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_actor_member_declarations(
        span_iter: &mut SpanIterator,
    ) -> Result<Vec<ActorMemberDeclaration>, ParseError> {
        let mut members = vec![];
        loop {
            if let Some(span) = span_iter.peek(0) {
                match span.token {
                    Token::Newline => {
                        // skip empty line.
                        span_iter.next();
                    }
                    Token::Event => members.push(ActorMemberDeclaration::Event(
                        Event::parse_event(span_iter)?,
                    )),
                    Token::Identifier { .. } => {
                        // parameter decl.
                        todo!("parameter declaration in actor is not supported");
                    }
                    Token::Var => {
                        // variable decl.
                        todo!("variable declaration in actor is not supported");
                    }
                    Token::Keep | Token::RemoveDefault => {
                        // constraint decl
                        members.push(ActorMemberDeclaration::Constraint(
                            Constraint::parse_constraint(span_iter)?,
                        ));
                    }
                    Token::Def => {
                        // method decl
                        members.push(ActorMemberDeclaration::Method(Method::parse_method(
                            span_iter,
                        )?));
                    }
                    Token::Cover | Token::Record => {
                        // coverage decl.
                        todo!("coverage declaration in actor is not supported");
                    }
                    Token::Dedent => {
                        // end of decl.
                        return Ok(members);
                    }
                    _ => {
                        return Err(ParseError {
                            error: ParseErrorType::ActorDeclarationError,
                            token_loc: Some(span.start_loc),
                        });
                    }
                }
            } else {
                // we expect at least one member declaration.
                if members.is_empty() {
                    return Err(ParseError {
                        error: ParseErrorType::ActorDeclarationError,
                        token_loc: None,
                    });
                }
                return Ok(members);
            }
        }
    }
}
