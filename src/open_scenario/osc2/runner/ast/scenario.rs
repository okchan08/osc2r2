#![allow(dead_code)]
use crate::open_scenario::osc2::runner::lex::token::Token;

use super::{actor::Actor, errors::ParseError, osc_struct::Struct, parser::Spans};

#[derive(Debug, Default)]
pub struct Scenario {
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
    Action,
    Modifier,
    TypeExtension,
    GlobalParameter,
}

impl Scenario {
    pub fn parse_scenario(spans: Spans) -> Result<Scenario, ParseError> {
        let mut scenario = Scenario::default();
        let mut span_iter = spans.iter();
        while let Some(span) = span_iter.next() {
            match span.token {
                Token::Actor => scenario.osc_declarations.push(OscDeclaration::Actor(
                    Actor::parse_actor_declaration(&mut span_iter)?,
                )),
                Token::Import => {
                    todo!("import is not supported yet")
                }
                Token::Newline => {
                    // skip empty line
                }
                Token::Struct => {
                    scenario
                        .osc_declarations
                        .push(OscDeclaration::Struct(Struct::parse_struct(
                            &mut span_iter,
                        )?))
                }
                _ => {
                    break;
                    //panic!("unexpected or unsupported token {} found", span.token)
                }
            }
        }
        Ok(scenario)
    }
}
