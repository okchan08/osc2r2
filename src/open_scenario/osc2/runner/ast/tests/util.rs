#![allow(dead_code)]
use crate::open_scenario::osc2::runner::ast::parser::Spans;
use crate::open_scenario::osc2::runner::lex::lexer::make_tokenizer;

pub fn lex_source(source: &str) -> Spans {
    let lexer = make_tokenizer(source, "".to_string());
    Spans::new(lexer.map(|x| x.unwrap()).collect())
}
