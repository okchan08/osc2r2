use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier { identifier: String },
    Indent,
    Dedent,
    Newline,
    EndOfFile,

    // Reserved operator and delimiter keywords
    Quotation,       // '
    DoubleQuotation, // "
    Period,          // .
    Comma,           // ,
    Colon,           // :
    Equal,           // =
    At,              // @
    Rarrow,          // ->
    Vbar,            // |
    Lpar,            // (
    Rpar,            // )
    Lsqb,            // [
    Rsqb,            // ]
    Question,        // ?
    REqArrow,        // =>
    And,             // and
    Or,              // or
    Not,             // not
    DoubleEq,        // ==
    NotEq,           //  !=
    Less,            // <
    LessEq,          // <=
    Greater,         // >
    GreaterEq,       // >=
    In,              // in
    Plus,            // +
    Minus,           // -
    Start,           // *
    Slash,           // /
    Percent,         // %

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
            Newline => f.write_str("newline"),
            EndOfFile => f.write_str("EOF"),

            Quotation => todo!(),
            DoubleQuotation => todo!(),
            Period => todo!(),
            Comma => f.write_str(","),
            Colon => f.write_str(":"),
            Equal => todo!(),
            At => todo!(),
            Rarrow => f.write_str("->"),
            Vbar => todo!(),
            Lpar => f.write_str("("),
            Rpar => f.write_str(")"),
            Lsqb => todo!(),
            Rsqb => todo!(),
            Question => todo!(),
            REqArrow => todo!(),
            And => todo!(),
            Or => todo!(),
            Not => todo!(),
            DoubleEq => todo!(),
            NotEq => todo!(),
            Less => todo!(),
            LessEq => todo!(),
            Greater => todo!(),
            GreaterEq => todo!(),
            In => todo!(),
            Plus => todo!(),
            Minus => todo!(),
            Start => todo!(),
            Slash => todo!(),
            Percent => todo!(),

            Action => {
                todo!()
            }
            Actor => f.write_str("actor"),
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
            Def => f.write_str("def"),
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
            Is => f.write_str("is"),
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
            Undefined => f.write_str("undefined"),
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
