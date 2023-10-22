use std::fmt;

use super::location::Location;

#[derive(Debug, PartialEq)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub location: Location,
}

#[derive(Debug, PartialEq)]
pub enum LexicalErrorType {
    TabsAfterSpaces,
    IndentationError,
    EOF,
}

impl fmt::Display for LexicalErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexicalErrorType::*;
        match self {
            TabsAfterSpaces => write!(f, "inconsistent use of tabs and spaces in indentatio"),
            IndentationError => write!(f, "unindent does not match any outer indentation level"),
            EOF => write!(f, "unexpected EOF while parsing"),
        }
    }
}
