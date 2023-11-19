use crate::open_scenario::osc2::runner::lex::token::Token;

use super::{
    behavior::BehaviorSpecification,
    errors::{ParseError, ParseErrorType},
    identifier::Identifier,
    parser::SpanIterator,
    scenario::ScenarioMemberDeclaration,
    utils,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Action {
    actor_name: Option<Identifier>,
    behavior_name: Identifier,
    scenarios: Vec<ScenarioMemberDeclaration>,
    behaviors: Vec<BehaviorSpecification>,
}

impl Action {
    pub fn parse_action(span_iter: &mut SpanIterator) -> Result<Action, ParseError> {
        let (actor_name, behavior_name) = utils::parse_qualified_behavior_name(span_iter)?;
        let mut action = Action {
            actor_name,
            behavior_name,
            scenarios: vec![],
            behaviors: vec![],
        };
        if let Some(span) = span_iter.peek(0) {
            match span.token {
                Token::Newline => {
                    // end of declaration
                    span_iter.next();
                    return Ok(action);
                }
                Token::Colon => {
                    // member declaration.
                    // expect colon + newline + indent before the declaration.
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
                    while let Some(span) = span_iter.peek(0) {
                        match span.token {
                            Token::Dedent => {
                                span_iter.next();
                                break;
                            }
                            Token::On | Token::Do => {
                                action.behaviors.push(
                                    BehaviorSpecification::parse_behavior_specification(span_iter)?,
                                );
                            }
                            _ => {
                                for scenario in
                                    ScenarioMemberDeclaration::parse_scenario_member_declarations(
                                        span_iter,
                                    )?
                                {
                                    action.scenarios.push(scenario);
                                }
                            }
                        }
                    }
                }
                Token::Inherits => {
                    return Err(ParseError {
                        error: ParseErrorType::Unsupported {
                            found: Token::Inherits,
                        },
                        token_loc: Some(span.start_loc),
                    });
                }
                _ => {
                    return Err(ParseError {
                        error: ParseErrorType::UnexpectedToken {
                            found: span.token.clone(),
                            expected: vec![Token::Newline, Token::Colon, Token::Inherits],
                        },
                        token_loc: Some(span.start_loc),
                    });
                }
            }
        }
        Ok(action)
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::ast::{
        field::{Field, Parameter, Variable},
        tests::util::lex_source,
        types::Type,
    };

    use super::*;

    #[test]
    pub fn test_parse_action() {
        let source = "
action actor_name.action_name:
  speed: speed
  var data1 int
";
        let spans = lex_source(source);
        let mut span_iter = spans.iter();
        span_iter.next(); // need to consume leading Action token
        let result = Action::parse_action(&mut span_iter).unwrap();

        assert_eq!(
            result.actor_name,
            Some(Identifier {
                name: "actor_name".to_string()
            })
        );
        assert_eq!(
            result.behavior_name,
            Identifier {
                name: "action_name".to_string()
            }
        );
        assert_eq!(result.scenarios.len(), 2);
        assert_eq!(
            result.scenarios[0],
            ScenarioMemberDeclaration::Field(Field::Parameter(Parameter {
                name: Identifier {
                    name: "speed".to_string()
                },
                field_type: Type::UserDefinedType(Identifier {
                    name: "speed".to_string()
                }),
                default_value: None,
                constraints: vec![],
            }))
        );
        assert_eq!(
            result.scenarios[1],
            ScenarioMemberDeclaration::Field(Field::Variable(Variable {
                name: Identifier {
                    name: "data1".to_string()
                },
                field_type: Type::Int,
                default_value: None,
                sample_expression: None,
            }))
        );
    }
    #[test]
    pub fn test_parse_action_without_actor() {
        let source = "
action action_name:
  speed: speed
  var data1 int
";
        let spans = lex_source(source);
        let mut span_iter = spans.iter();
        span_iter.next(); // need to consume leading Action token
        let result = Action::parse_action(&mut span_iter).unwrap();

        assert_eq!(result.actor_name, None);
        assert_eq!(
            result.behavior_name,
            Identifier {
                name: "action_name".to_string()
            }
        );
        assert_eq!(result.scenarios.len(), 2);
        assert_eq!(
            result.scenarios[0],
            ScenarioMemberDeclaration::Field(Field::Parameter(Parameter {
                name: Identifier {
                    name: "speed".to_string()
                },
                field_type: Type::UserDefinedType(Identifier {
                    name: "speed".to_string()
                }),
                default_value: None,
                constraints: vec![],
            }))
        );
        assert_eq!(
            result.scenarios[1],
            ScenarioMemberDeclaration::Field(Field::Variable(Variable {
                name: Identifier {
                    name: "data1".to_string()
                },
                field_type: Type::Int,
                default_value: None,
                sample_expression: None,
            }))
        );
    }
}
