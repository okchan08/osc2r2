#![allow(dead_code)]

use crate::open_scenario::osc2::runner::{
    ast::identifier::Identifier,
    lex::{lexer::Spanned, token::Token},
};

use super::{
    action::Action,
    actor::Actor,
    constraint::Constraint,
    errors::{ParseError, ParseErrorType},
    event::Event,
    field::Field,
    method::Method,
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
    Enum,
    Struct(Struct),
    Actor(Actor),
    Action(Action),
    Modifier,
    TypeExtension,
    GlobalParameter,
    Scenario,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum ScenarioMemberDeclaration {
    Event(Event),
    Field(Field),
    Constraint(Constraint),
    Method(Method),
    Coverage,
    Modifier,
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
                _ => {
                    panic!("unexpected or unsupported token {} found", span.token)
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
        loop {
            if let Some(span) = span_iter.peek(0) {
                match span.token {
                    Token::Newline => {
                        // skip empty line.
                        span_iter.next();
                    }
                    Token::Event => results.push(ScenarioMemberDeclaration::Event(
                        Event::parse_event(span_iter)?,
                    )),
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
                    _ => {
                        return Err(ParseError {
                            error: ParseErrorType::ScenarioDeclarationError,
                            token_loc: Some(span.start_loc),
                        });
                    }
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
        }
    }
}
