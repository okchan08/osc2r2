use colored::Colorize;
use std::fmt::{self, Debug};

use super::location::Location;

#[derive(PartialEq)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub location: Location,
    pub filename: String,
}

impl Debug for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            format!(
                "[ERROR] line:{} col:{} in {}",
                self.location.row(),
                self.location.column(),
                self.filename
            )
            .red()
            .bold()
            .to_string()
            .as_str(),
        )?;
        f.write_str(format!(": {}", self.error).as_str())
    }
}

#[derive(Debug, PartialEq)]
pub enum LexicalErrorType {
    TabsAfterSpaces,
    IndentationError,
    EOF,
    NotSupportedYet,
    OtherError(String),
}

impl fmt::Display for LexicalErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexicalErrorType::*;
        match self {
            TabsAfterSpaces => write!(f, "inconsistent use of tabs and spaces in indentatio"),
            IndentationError => write!(f, "unindent does not match any outer indentation level"),
            EOF => write!(f, "unexpected EOF while parsing"),
            NotSupportedYet => write!(f, "not supported yet"),
            OtherError(msg) => write!(f, "{}", msg),
        }
    }
}
