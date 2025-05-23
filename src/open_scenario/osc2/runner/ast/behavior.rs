#![allow(dead_code)]
use crate::open_scenario::osc2::runner::lex::{lexer::Spanned, token::Token};

use super::{
    argument::ArgumentList,
    constraint::Constraint,
    errors::{ParseError, ParseErrorType},
    identifier::Identifier,
    parser::SpanIterator,
    utils,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum BehaviorSpecification {
    Do(DoDirective),
    On(OnDirective),
}

impl BehaviorSpecification {
    pub fn parse_behavior_specification(
        span_iter: &mut SpanIterator,
    ) -> Result<BehaviorSpecification, ParseError> {
        let Some(span) = span_iter.peek(0) else {
          return Err(ParseError { error: ParseErrorType::EndOfFile, token_loc: None });
        };
        match span.token {
            Token::On => {
                todo!("on directive is not supported yet")
            }
            Token::Do => Ok(BehaviorSpecification::Do(DoDirective::parse_do_directive(
                span_iter,
            )?)),
            _ => Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    found: span.token.clone(),
                    expected: vec![Token::Do, Token::On],
                },
                token_loc: Some(span.start_loc),
            }),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct DoDirective {
    label_name: Option<Identifier>,
    do_member: DoMember,
}

impl DoDirective {
    pub fn parse_do_directive(span_iter: &mut SpanIterator) -> Result<DoDirective, ParseError> {
        span_iter.next(); // consume Do token
        let label_name = DoDirective::parse_label(span_iter)?;

        Ok(DoDirective {
            label_name,
            do_member: DoDirective::parse_do_member(span_iter)?,
        })
    }

    fn parse_label(span_iter: &mut SpanIterator) -> Result<Option<Identifier>, ParseError> {
        if let (Some(span1), Some(span2)) = (span_iter.peek(0), span_iter.peek(1)) {
            match (&span1.token, &span2.token) {
                (Token::Identifier { identifier }, Token::Colon) => {
                    return Ok(Some(Identifier {
                        name: identifier.to_owned(),
                    }));
                }
                _ => {
                    return Ok(None);
                }
            };
        };
        Ok(None)
    }

    fn parse_do_member(span_iter: &mut SpanIterator) -> Result<DoMember, ParseError> {
        if let Some(span) = span_iter.peek(0) {
            match &span.token {
                Token::Serial | Token::Parallel | Token::OneOf => Ok(DoMember::Composition(
                    Composition::parse_composition(span_iter)?,
                )),
                Token::Wait | Token::Call => Err(ParseError {
                    error: ParseErrorType::Unsupported {
                        found: span.token.clone(),
                    },
                    token_loc: Some(span.start_loc),
                }),
                Token::Identifier { identifier: _ } => Ok(DoMember::BehaviorInvocation(
                    BehaviorInvocation::parse_behavior_invocation(span_iter)?,
                )),
                Token::Emit => {
                    span_iter.next();
                    let event_name = utils::parse_identifier(span_iter)?;
                    if let Some(span) = span_iter.next() {
                        match &span.token {
                            Token::Newline => Ok(DoMember::EmitDirective(Emit { event_name })),
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
                _ => Err(ParseError {
                    error: ParseErrorType::UnexpectedToken {
                        found: span.token.clone(),
                        expected: vec![
                            Token::Serial,
                            Token::Parallel,
                            Token::OneOf,
                            Token::Wait,
                            Token::Call,
                            Token::Identifier {
                                identifier: "".to_string(),
                            },
                            Token::Emit,
                        ],
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum DoMember {
    Composition(Composition),
    BehaviorInvocation(BehaviorInvocation),
    WaitDirective,
    EmitDirective(Emit),
    CallDirective,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum CompositionOp {
    Serial,
    Parallel,
    OneOf,
}

impl TryFrom<&Spanned> for CompositionOp {
    type Error = ParseError;

    fn try_from(value: &Spanned) -> Result<Self, Self::Error> {
        match value.token {
            Token::Serial => Ok(CompositionOp::Serial),
            Token::Parallel => Ok(CompositionOp::Parallel),
            Token::OneOf => Ok(CompositionOp::OneOf),
            _ => Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    found: value.token.clone(),
                    expected: vec![Token::Serial, Token::Parallel, Token::OneOf],
                },
                token_loc: Some(value.start_loc),
            }),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Composition {
    operator: CompositionOp,
    args: Option<ArgumentList>,
    members: Vec<DoMember>,
}

impl Composition {
    fn parse_composition(span_iter: &mut SpanIterator) -> Result<Composition, ParseError> {
        let composit_op = CompositionOp::try_from(span_iter.next().unwrap())?;
        let mut composition = Composition {
            operator: composit_op,
            args: None,
            members: vec![],
        };

        match span_iter.next() {
            Some(Spanned {
                token: Token::Lpar, ..
            }) => {
                let args = ArgumentList::parse_arguments(span_iter)?;
                utils::consume_one_token(span_iter, Token::Rpar)?;
                composition.args = Some(args);
            }
            Some(Spanned {
                token: Token::Colon,
                ..
            }) => {
                utils::consume_one_token(span_iter, Token::Newline)?;
                utils::consume_one_token(span_iter, Token::Indent)?;
                loop {
                    if let Some(span) = span_iter.peek(0) {
                        if matches!(span.token, Token::Dedent) {
                            span_iter.next();
                            break;
                        }
                        if matches!(span.token, Token::Newline) {
                            span_iter.next();
                            continue;
                        }
                        composition
                            .members
                            .push(DoDirective::parse_do_member(span_iter)?);
                    }
                }
            }
            Some(span) => {
                return Err(ParseError {
                    error: ParseErrorType::UnexpectedToken {
                        found: span.token.clone(),
                        expected: vec![Token::Lpar, Token::Colon],
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
        Ok(composition)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct BehaviorInvocation {
    actor_name: Option<Identifier>,
    name: Identifier,
    with_member: Vec<BehaviorWithMember>,
    args: Option<ArgumentList>,
}

impl BehaviorInvocation {
    pub fn parse_behavior_invocation(
        span_iter: &mut SpanIterator,
    ) -> Result<BehaviorInvocation, ParseError> {
        let (actor_name, name) = utils::parse_qualified_behavior_name(span_iter)?;
        if name.name == "run_car" {
            println!("{:?} {:?}", actor_name, name);
            println!("{:?}", span_iter.peek(0));
        }
        let mut behavior_invocation = BehaviorInvocation {
            actor_name,
            name,
            with_member: vec![],
            args: None,
        };
        utils::consume_one_token(span_iter, Token::Lpar)?;
        if utils::match_peek_next(span_iter, Token::Rpar) {
            // no argument list
            span_iter.next();
        } else {
            let args = ArgumentList::parse_arguments(span_iter)?;
            behavior_invocation.args = Some(args);
            utils::consume_one_token(span_iter, Token::Rpar)?;
        }

        if let Some(span) = span_iter.peek(0) {
            match span.token {
                Token::With => {
                    span_iter.next();
                    utils::consume_one_token(span_iter, Token::Colon)?;
                    utils::consume_one_token(span_iter, Token::Newline)?;
                    utils::consume_one_token(span_iter, Token::Indent)?;
                    behavior_invocation.with_member =
                        BehaviorWithMember::parse_behavior_with_members(span_iter)?;
                }
                Token::Newline => {
                    span_iter.next();
                }
                _ => {
                    return Err(ParseError {
                        error: ParseErrorType::UnexpectedToken {
                            found: span.token.clone(),
                            expected: vec![Token::With, Token::Newline],
                        },
                        token_loc: Some(span.start_loc),
                    });
                }
            }
        }

        let Some(_) = span_iter.peek(0) else {
          return Err(ParseError {
            error: ParseErrorType::EndOfFile,
            token_loc: None
          })
        };

        Ok(behavior_invocation)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Emit {
    event_name: Identifier,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum BehaviorWithMember {
    Constraint(Constraint),
    Modifier,
    Until,
}

impl BehaviorWithMember {
    pub fn parse_behavior_with_members(
        span_iter: &mut SpanIterator,
    ) -> Result<Vec<BehaviorWithMember>, ParseError> {
        let mut results = vec![];
        loop {
            if let Some(span) = span_iter.peek(0) {
                match span.token {
                    Token::Keep | Token::RemoveDefault => {
                        results.push(BehaviorWithMember::Constraint(
                            Constraint::parse_constraint(span_iter)?,
                        ));
                    }
                    Token::Newline => {
                        // consume empty line
                        span_iter.next();
                    }
                    Token::Dedent => {
                        // end of with member decl.
                        span_iter.next();
                        break;
                    }
                    _ => {
                        todo!("modifier application or until directive not supported yet")
                    }
                }
            }
        }
        Ok(results)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct OnDirective {
    event_name: Identifier,
    on_members: Vec<OnDirective>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum OnMember {
    CallDirective,
    EmitDirective(Emit),
}
