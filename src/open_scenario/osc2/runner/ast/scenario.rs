use crate::open_scenario::osc2::runner::lex::token::Token;

use super::{actor::Actor, errors::ParseError, parser::Spans};

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
    PhysicalTypeDeclaration,
    #[default]
    UnitDeclaration,
    EnumDeclaration,
    StructDeclaration,
    ActorDeclaration(Actor),
    ActionDeclaration,
    ScenarioDeclaration,
    ModifierDeclaration,
    TypeExtension,
    GlobalParameterDeclaration,
}

impl Scenario {
    pub fn parse_scenario(spans: Spans) -> Result<Scenario, ParseError> {
        let mut scenario = Scenario::default();
        let mut span_iter = spans.iter();
        while let Some(span) = span_iter.next() {
            match span.token {
                Token::Actor => scenario
                    .osc_declarations
                    .push(OscDeclaration::ActorDeclaration(
                        Actor::parse_actor_declaration(&mut span_iter)?,
                    )),
                Token::Import => {
                    todo!("import is not supported yet")
                }
                Token::Newline => {
                    // skip empty line
                    println!("skip empty line")
                }
                _ => {
                    panic!("unexpected or unsupported token {} found", span.token)
                }
            }
        }
        Ok(scenario)
    }
}
