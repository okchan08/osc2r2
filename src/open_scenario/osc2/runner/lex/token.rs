use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier { identifier: String },
    Number { num: String },
    Indent,
    Dedent,
    Newline,
    EndOfFile,

    // Reserved operator and delimiter keywords
    Quotation,       // '
    DoubleQuotation, // "
    Period,          // .
    DoublePeriod,    // ..
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
    Star,            // *
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
            Number { num } => write!(f, "{}", num),
            Indent => f.write_str("indent"),
            Dedent => f.write_str("dedent"),
            Newline => f.write_str("newline"),
            EndOfFile => f.write_str("EOF"),
            Quotation => f.write_str("'"),
            DoubleQuotation => f.write_str("\""),
            Period => f.write_str("."),
            DoublePeriod => f.write_str(".."),
            Comma => f.write_str(","),
            Colon => f.write_str(":"),
            Equal => f.write_str("="),
            At => f.write_str("@"),
            Rarrow => f.write_str("->"),
            Vbar => f.write_str("|"),
            Lpar => f.write_str("("),
            Rpar => f.write_str(")"),
            Lsqb => f.write_str("["),
            Rsqb => f.write_str("]"),
            Question => f.write_str("?"),
            REqArrow => f.write_str("=>"),
            And => f.write_str("and"),
            Or => f.write_str("or"),
            Not => f.write_str("not"),
            DoubleEq => f.write_str("=="),
            NotEq => f.write_str("!="),
            Less => f.write_str("<"),
            LessEq => f.write_str("<="),
            Greater => f.write_str(">"),
            GreaterEq => f.write_str(">="),
            In => f.write_str("in"),
            Plus => f.write_str("+"),
            Minus => f.write_str("-"),
            Star => f.write_str("*"),
            Slash => f.write_str("/"),
            Percent => f.write_str("%"),

            Action => f.write_str("action"),
            Actor => f.write_str("actor"),
            As => f.write_str("as"),
            Bool => f.write_str("bool"),
            Call => f.write_str("call"),
            Cover => f.write_str("cover"),
            Def => f.write_str("def"),
            Default => f.write_str("default"),
            Do => f.write_str("do"),
            Elapsed => f.write_str("elapsed"),
            Emit => f.write_str("emit"),
            Enum => f.write_str("enum"),
            Event => f.write_str("event"),
            Every => f.write_str("every"),
            Expression => f.write_str("expression"),
            Extend => f.write_str("extend"),
            External => f.write_str("external"),
            Fall => f.write_str("fall"),
            Float => f.write_str("float"),
            Global => f.write_str("global"),
            Hard => f.write_str("hard"),
            If => f.write_str("if"),
            Import => f.write_str("import"),
            Inherits => f.write_str("inherits"),
            Int => f.write_str("int"),
            Is => f.write_str("is"),
            It => f.write_str("it"),
            Keep => f.write_str("keep"),
            List => f.write_str("list"),
            Of => f.write_str("of"),
            On => f.write_str("on"),
            OneOf => f.write_str("one_of"),
            Only => f.write_str("only"),
            Parallel => f.write_str("parallel"),
            Range => f.write_str("range"),
            Record => f.write_str("record"),
            RemoveDefault => f.write_str("remove_default"),
            Rise => f.write_str("rise"),
            Scenario => f.write_str("scenario"),
            Serial => f.write_str("serial"),
            SI => f.write_str("SI"),
            String => f.write_str("string"),
            Struct => f.write_str("struct"),
            Type => f.write_str("type"),
            Undefined => f.write_str("undefined"),
            Uint => f.write_str("uint"),
            Unit => f.write_str("unit"),
            Until => f.write_str("until"),
            Var => f.write_str("var"),
            Wait => f.write_str("wait"),
            With => f.write_str("with"),
        }
    }
}
