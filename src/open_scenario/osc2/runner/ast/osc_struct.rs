use bevy::utils::HashMap;

use crate::open_scenario::osc2::runner::lex::token::Token;

use super::{
    errors::{ParseError, ParseErrorType},
    identifier::Identifier,
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

#[derive(Debug, Default)]
struct StructMemberDeclaration {}

impl Struct {
    pub fn parse_struct(span_iter: &mut SpanIterator) -> Result<Struct, ParseError> {
        let mut osc_struct = Struct::default();
        let struct_name = Self::parse_struct_name(span_iter)?;
        todo!()
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
