use std::collections::HashSet;

use crate::open_scenario::osc2::runner::lex::token::Token;

use super::{
    errors::{ParseError, ParseErrorType},
    expression::value::ValueExpression,
    identifier::Identifier,
    parser::SpanIterator,
    utils,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct OscEnum {
    name: Identifier,
    enum_members: Vec<OscEnumMember>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct OscEnumMember {
    name: Identifier,
    val: u64,
}

impl OscEnum {
    pub fn parse_enum(span_iter: &mut SpanIterator) -> Result<OscEnum, ParseError> {
        let enum_name = utils::parse_identifier(span_iter)?;
        let mut osc_enum = OscEnum {
            name: enum_name,
            enum_members: vec![],
        };
        utils::consume_one_token(span_iter, Token::Colon)?;
        utils::consume_one_token(span_iter, Token::Lsqb)?;

        let mut default_val_checker = HashSet::<u64>::new();
        loop {
            let first_enum_member_name = utils::parse_identifier(span_iter)?;
            if utils::match_peek_next(span_iter, Token::Equal) {
                // found default val.
                utils::consume_one_token(span_iter, Token::Equal)?;
                let val = ValueExpression::parse_uint_literal(span_iter)?;
                if default_val_checker.contains(&val) {
                    return Err(ParseError {
                        error: ParseErrorType::DuplicateDefinition(
                            "duplication enum value found".to_string(),
                        ),
                        token_loc: Some(span_iter.peek(0).unwrap().start_loc),
                    });
                }
                default_val_checker.insert(val);
                osc_enum.enum_members.push(OscEnumMember {
                    name: first_enum_member_name,
                    val,
                })
            } else {
                let val = default_val_checker.len();
                osc_enum.enum_members.push(OscEnumMember {
                    name: first_enum_member_name,
                    val: val as u64,
                });
                default_val_checker.insert(val as u64);
            }
            if utils::match_peek_next(span_iter, Token::Colon) {
                utils::consume_one_token(span_iter, Token::Colon)?;
            } else if utils::match_peek_next(span_iter, Token::Rsqb) {
                utils::consume_one_token(span_iter, Token::Rsqb)?;
                break;
            }
        }
        utils::consume_one_token(span_iter, Token::Newline)?;

        Ok(osc_enum)
    }
}
