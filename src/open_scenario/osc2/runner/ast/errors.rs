use colored::Colorize;
use std::fmt;

use crate::open_scenario::osc2::runner::lex::{location::Location, token::Token};

#[derive(Debug, Default)]
pub struct ParseError {
    pub error: ParseErrorType,
    pub token_loc: Option<Location>,
}

impl ParseError {
    pub fn display_error(&self, source_code: String) {
        let lines = source_code.split('\n').collect::<Vec<&str>>();
        if let Some(error_loc) = self.token_loc {
            println!(
                "{}",
                format!(
                    "[ERROR]: line:{} col:{}",
                    error_loc.row(),
                    error_loc.column()
                )
                .red()
                .bold()
            );
            print!("\n");
            println!("{}", lines[error_loc.row() - 1]);
            println!(
                "{}",
                format!(
                    "{}^ {}",
                    " ".repeat(error_loc.column()),
                    self.error.to_string()
                )
                .red()
                .bold()
            );
        }
    }
}

#[derive(Debug, Default)]
pub enum ParseErrorType {
    ActorDeclarationError,
    EndOfFile,
    UnexpectedToken {
        found: Token,
        expected: Vec<Token>,
    },
    #[default]
    Unsupported,
}

impl fmt::Display for ParseErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParseErrorType::*;
        match self {
            ActorDeclarationError => f.write_str("actor declaration error"),
            EndOfFile => f.write_str("unexpected EOF"),
            UnexpectedToken { found, expected } => f.write_str(
                format!(
                    "expected {}, found {}",
                    //found.to_string(),
                    expected
                        .into_iter()
                        .map(|token| token.to_string())
                        .collect::<Vec<String>>()
                        .join(","),
                    found.to_string(),
                )
                .as_str(),
            ),
            Unsupported => f.write_str("not supported yet."),
        }
    }
}
