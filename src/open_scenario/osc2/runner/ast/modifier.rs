use crate::open_scenario::osc2::runner::{
    ast::{
        errors::ParseErrorType,
        utils::{self, parse_qualified_behavior_name},
    },
    lex::{lexer::Spanned, token::Token},
};

use super::{
    behavior::OnDirective, errors::ParseError, identifier::Identifier, parser::SpanIterator,
    scenario::ScenarioMemberDeclaration,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Modifier {
    actor_name: Option<Identifier>,
    modifier_name: Identifier,
    qualified_name: Option<(Option<Identifier>, Identifier)>,
    scenarios: Vec<ScenarioMemberDeclaration>,
    on_directives: Vec<OnDirective>,
}

impl Modifier {
    pub fn parse_modifier(span_iter: &mut SpanIterator) -> Result<Modifier, ParseError> {
        let (actor_name, modifier_name) = utils::parse_qualified_behavior_name(span_iter)?;
        let mut modifier = Modifier {
            actor_name,
            modifier_name,
            qualified_name: None,
            scenarios: vec![],
            on_directives: vec![],
        };
        modifier.qualified_name = Modifier::parse_of_qualified_behavior(span_iter)?;
        if let Some(span) = span_iter.peek(0) {
            match span.token {
                Token::Colon => {
                    // member declaration.
                    // expect colon + newline + indent
                    span_iter.next(); // consume colon.
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
                    loop {
                        if let Some(span) = span_iter.peek(0) {
                            match span.token {
                                Token::Dedent => {
                                    span_iter.next();
                                    break;
                                }
                                Token::On => {
                                    todo!("on directive is not supported.");
                                }
                                _ => {
                                    for scenario in ScenarioMemberDeclaration::parse_scenario_member_declarations(span_iter)? {
                                    modifier.scenarios.push(scenario);
                                  }
                                }
                            }
                        } else {
                            break;
                        }
                    }
                }
                _ => {
                    return Err(ParseError {
                        error: ParseErrorType::UnexpectedToken {
                            found: span.token.clone(),
                            expected: vec![Token::Newline, Token::Colon, Token::Inherits],
                        },
                        token_loc: Some(span.start_loc.clone()),
                    });
                }
            };
        }
        Ok(modifier)
    }

    fn parse_of_qualified_behavior(
        span_iter: &mut SpanIterator,
    ) -> Result<Option<(Option<Identifier>, Identifier)>, ParseError> {
        // parse `modifier my_mod of actor_name.behavior_name` and extract actor_name and behavior name.
        // after this function span_iter points to the next token of "behavior_name".
        match span_iter.peek(0) {
            Some(Spanned {
                token: Token::Of, ..
            }) => {
                span_iter.next(); // consume Of token.
                Ok(Some(parse_qualified_behavior_name(span_iter)?))
            }
            _ => Ok(None),
        }
    }
}
