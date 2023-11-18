#![allow(dead_code)]

use bevy::utils::tracing::Span;

use crate::open_scenario::osc2::runner::{
    ast::{identifier::Identifier, utils},
    lex::{lexer::Spanned, token::Token},
};

use super::{
    action::Action,
    actor::Actor,
    behavior::{BehaviorInvocation, BehaviorSpecification},
    constraint::Constraint,
    errors::{ParseError, ParseErrorType},
    event::Event,
    field::Field,
    method::Method,
    modifier::Modifier,
    osc_enum::OscEnum,
    osc_struct::Struct,
    parser::{SpanIterator, Spans},
};

#[derive(Debug, Default)]
pub struct OscFile {
    import_statements: Vec<ImportStatement>,
    osc_declarations: Vec<OscDeclaration>,
}

#[derive(Debug, Default)]
struct ImportStatement {
    import_path: String,
}

#[derive(Debug, Default)]
enum OscDeclaration {
    PhysicalType,
    #[default]
    Unit,
    Enum(OscEnum),
    Struct(Struct),
    Actor(Actor),
    Action(Action),
    Modifier(Modifier),
    TypeExtension,
    GlobalParameter,
    Scenario(Scenario),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum ScenarioMemberDeclaration {
    Event(Event),
    Field(Field),
    Constraint(Constraint),
    Method(Method),
    Coverage,
    Modifier(Modifier),
}

impl OscFile {
    pub fn parse_scenario_file(spans: Spans) -> Result<OscFile, ParseError> {
        let mut osc_file = OscFile::default();
        let mut span_iter = spans.iter();
        while let Some(span) = span_iter.next() {
            match span.token {
                Token::Actor => osc_file.osc_declarations.push(OscDeclaration::Actor(
                    Actor::parse_actor_declaration(&mut span_iter)?,
                )),
                Token::Import => {
                    todo!("import is not supported yet")
                }
                Token::Newline => {
                    // skip empty line
                }
                Token::Struct => {
                    osc_file
                        .osc_declarations
                        .push(OscDeclaration::Struct(Struct::parse_struct(
                            &mut span_iter,
                        )?))
                }
                Token::Action => {
                    osc_file
                        .osc_declarations
                        .push(OscDeclaration::Action(Action::parse_action(
                            &mut span_iter,
                        )?))
                }
                Token::Modifier => {
                    osc_file.osc_declarations.push(OscDeclaration::Modifier(
                        Modifier::parse_modifier(&mut span_iter)?,
                    ));
                }
                Token::Scenario => osc_file.osc_declarations.push(OscDeclaration::Scenario(
                    Scenario::parse_scenario(&mut span_iter)?,
                )),
                Token::Enum => {
                    osc_file
                        .osc_declarations
                        .push(OscDeclaration::Enum(OscEnum::parse_enum(&mut span_iter)?));
                }
                _ => {
                    panic!("unexpected or unsupported token {:?} found at", span.token)
                }
            }
        }
        Ok(osc_file)
    }
}

impl ScenarioMemberDeclaration {
    pub fn parse_scenario_member_declarations(
        span_iter: &mut SpanIterator,
    ) -> Result<Vec<ScenarioMemberDeclaration>, ParseError> {
        let mut results = vec![];
        if let Some(span) = span_iter.peek(0) {
            match span.token {
                Token::Newline => {
                    // skip empty line.
                    span_iter.next();
                }
                Token::Event => results.push(ScenarioMemberDeclaration::Event(Event::parse_event(
                    span_iter,
                )?)),
                Token::Identifier { .. } | Token::Var => {
                    // field decl.
                    for field in Field::parse_fields(span_iter)?.into_iter() {
                        results.push(ScenarioMemberDeclaration::Field(field));
                    }
                }
                Token::Keep | Token::RemoveDefault => {
                    // constraint decl
                    results.push(ScenarioMemberDeclaration::Constraint(
                        Constraint::parse_constraint(span_iter)?,
                    ));
                }
                Token::Def => {
                    // method decl
                    results.push(ScenarioMemberDeclaration::Method(Method::parse_method(
                        span_iter,
                    )?));
                }
                Token::Cover | Token::Record => {
                    // coverage decl.
                    todo!("coverage declaration in actor is not supported");
                }
                Token::Dedent => {
                    // end of decl.
                    return Ok(results);
                }
                _ => {}
            }
        } else {
            // we expect at least one member declaration.
            if results.is_empty() {
                return Err(ParseError {
                    error: ParseErrorType::ScenarioDeclarationError,
                    token_loc: None,
                });
            }
            return Ok(results);
        }
        Ok(results)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Scenario {
    associated_actor_name: Option<Identifier>,
    name: Identifier,
    scenarios: Vec<ScenarioMemberDeclaration>,
    behaviors: Vec<BehaviorSpecification>,
}

impl Scenario {
    pub fn parse_scenario(span_iter: &mut SpanIterator) -> Result<Scenario, ParseError> {
        let (associated_actor_name, name) = utils::parse_qualified_behavior_name(span_iter)?;
        let mut scenario = Scenario {
            associated_actor_name,
            name,
            scenarios: vec![],
            behaviors: vec![],
        };
        if let Some(span) = span_iter.peek(0) {
            match span.token {
                Token::Newline => {
                    // end of declaration
                    span_iter.next();
                    return Ok(scenario);
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
                                Token::On | Token::Do => {
                                    scenario.behaviors.push(
                                        BehaviorSpecification::parse_behavior_specification(
                                            span_iter,
                                        )?,
                                    );
                                }
                                _ => {
                                    for member in ScenarioMemberDeclaration::parse_scenario_member_declarations(span_iter)? {
                                        scenario.scenarios.push(member);
                                  }
                                }
                            }
                        } else {
                            break;
                        }
                    }
                }
                Token::Inherits => {
                    return Err(ParseError {
                        error: ParseErrorType::Unsupported {
                            found: Token::Inherits,
                        },
                        token_loc: Some(span.start_loc.clone()),
                    });
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
            }
        }
        Ok(scenario)
    }
}
