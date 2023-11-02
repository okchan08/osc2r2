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
            println!();
            println!("{}", lines[error_loc.row() - 1]);
            println!(
                "{}",
                format!("{}^ {}", " ".repeat(error_loc.column()), self.error)
                    .red()
                    .bold()
            );
        }
    }
}

#[derive(Debug, Default)]
pub enum ParseErrorType {
    ActorDeclarationError,
    ActionDeclarationError,
    ScenarioDeclarationError,
    #[default]
    EndOfFile,
    UnexpectedToken {
        found: Token,
        expected: Vec<Token>,
    },
    Unsupported {
        found: Token,
    },
}

impl fmt::Display for ParseErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParseErrorType::*;
        match self {
            ActorDeclarationError => f.write_str("actor declaration error"),
            ActionDeclarationError => f.write_str("action declaration error"),
            ScenarioDeclarationError => f.write_str("scenario declaration error"),
            EndOfFile => f.write_str("unexpected EOF"),
            UnexpectedToken { found, expected } => f.write_str(
                format!(
                    "expected {}, found {}",
                    expected
                        .iter()
                        .map(|token| token.to_string())
                        .collect::<Vec<String>>()
                        .join(","),
                    found,
                )
                .as_str(),
            ),
            Unsupported { found } => f.write_str(format!("{} not supported yet.", found).as_str()),
        }
    }
}
