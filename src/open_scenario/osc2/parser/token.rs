use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier { identifier: String },
    Indent,
    Dedent,
    Newline,
    EndOfFile,
    Colon,
    Comma,
    Rpar,
    Lpar,
    Rarrow,
    Semicolon,
    // Reserved keywords
    Action,
    Actor,
    As,
    Bool,
    Call,
    Cover,
    Def,
    Default,
    Do,
    Elapsed,
    Emit,
    Enum,
    Event,
    Every,
    Expression,
    Extend,
    External,
    Fall,
    Float,
    Global,
    Hard,
    If,
    Import,
    Inherits,
    Int,
    Is,
    It,
    Keep,
    List,
    Of,
    On,
    OneOf,
    Only,
    Parallel,
    Range,
    Record,
    RemoveDefault,
    Rise,
    Scenario,
    Serial,
    SI,
    String,
    Struct,
    Type,
    Uint,
    Undefined,
    Unit,
    Until,
    Var,
    Wait,
    With,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Identifier { identifier } => write!(f, "'{}'", identifier),
            Indent => f.write_str("indent"),
            Dedent => f.write_str("dedent"),
            Actor => f.write_str("actor"),
            Newline => f.write_str("newline"),
            EndOfFile => f.write_str("EOF"),
            Colon => f.write_str(":"),
            Comma => f.write_str(","),
            Def => f.write_str("def"),
            Rpar => f.write_str(")"),
            Lpar => f.write_str("("),
            Is => f.write_str("is"),
            Undefined => f.write_str("undefined"),
            Rarrow => f.write_str("->"),
            Semicolon => f.write_str(";"),
            Action => {
                todo!()
            }
            As => {
                todo!()
            }
            Bool => {
                todo!()
            }
            Call => {
                todo!()
            }
            Cover => {
                todo!()
            }
            Default => {
                todo!()
            }
            Do => {
                todo!()
            }
            Elapsed => {
                todo!()
            }
            Emit => {
                todo!()
            }
            Enum => {
                todo!()
            }
            Event => {
                todo!()
            }
            Every => {
                todo!()
            }
            Expression => {
                todo!()
            }
            Extend => {
                todo!()
            }
            External => {
                todo!()
            }
            Fall => {
                todo!()
            }
            Float => {
                todo!()
            }
            Global => {
                todo!()
            }
            Hard => {
                todo!()
            }
            If => {
                todo!()
            }
            Import => {
                todo!()
            }
            Inherits => {
                todo!()
            }
            Int => {
                todo!()
            }
            It => {
                todo!()
            }
            Keep => {
                todo!()
            }
            List => {
                todo!()
            }
            Of => {
                todo!()
            }
            On => {
                todo!()
            }
            OneOf => {
                todo!()
            }
            Only => {
                todo!()
            }
            Parallel => {
                todo!()
            }
            Range => {
                todo!()
            }
            Record => {
                todo!()
            }
            RemoveDefault => {
                todo!()
            }
            Rise => {
                todo!()
            }
            Scenario => {
                todo!()
            }
            Serial => {
                todo!()
            }
            SI => {
                todo!()
            }
            String => {
                todo!()
            }
            Struct => {
                todo!()
            }
            Type => {
                todo!()
            }
            Uint => {
                todo!()
            }
            Unit => {
                todo!()
            }
            Until => {
                todo!()
            }
            Var => {
                todo!()
            }
            Wait => {
                todo!()
            }
            With => {
                todo!()
            }
        }
    }
}
