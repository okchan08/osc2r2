use phf::phf_map;

use super::token::Token;

pub(super) static ASCII_LOWER: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z',
];

pub(super) static ASCII_UPPER: [char; 26] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

// list of reserved keywords
pub(super) static KEYWORD_MAP: phf::Map<&'static str, Token> = phf_map! {
  "action" => Token::Action,
  "actor" =>  Token::Actor,
  "as" =>  Token::As,
  "bool" =>  Token::Bool,
  "call" =>  Token::Call,
  "cover" =>  Token::Cover,
  "def" =>  Token::Def,
  "default" =>  Token::Default,
  "do" =>  Token::Do,
  "elapsed" =>  Token::Elapsed,
  "emit" =>  Token::Emit,
  "enum" =>  Token::Enum,
  "event" =>  Token::Event,
  "every" =>  Token::Every,
  "expression" =>  Token::Expression,
  "extend" =>  Token::Extend,
  "external" =>  Token::External,
  "fall" =>  Token::Fall,
  "false" => Token::False,
  "float" =>  Token::Float,
  "global" =>  Token::Global,
  "hard" =>  Token::Hard,
  "if" =>  Token::If,
  "import" =>  Token::Import,
  "inherits" =>  Token::Inherits,
  "int" =>  Token::Int,
  "is" =>  Token::Is,
  "it" =>  Token::It,
  "keep" =>  Token::Keep,
  "list" =>  Token::List,
  "modifier" => Token::Modifier,
  "of" =>  Token::Of,
  "on" =>  Token::On,
  "one_of" =>  Token::OneOf,
  "only" =>  Token::Only,
  "parallel" =>  Token::Parallel,
  "range" =>  Token::Range,
  "record" =>  Token::Record,
  "remove_default" =>  Token::RemoveDefault,
  "rize" =>  Token::Rise,
  "sample" => Token::Sample,
  "scenario" =>  Token::Scenario,
  "serial" =>  Token::Serial,
  "SI" =>  Token::SI,
  "string" =>  Token::String,
  "struct" =>  Token::Struct,
  "type" =>  Token::Type,
  "true" => Token::True,
  "undefined" =>  Token::Undefined,
  "unit" =>  Token::Unit,
  "until" =>  Token::Until,
  "var" =>  Token::Var,
  "wait" =>  Token::Wait,
  "with" =>  Token::With,
  "'" =>  Token::Quotation,
  "\"" =>  Token::DoubleQuotation,
  "." =>  Token::Period,
  ".." =>  Token::DoublePeriod,
  "," =>  Token::Comma,
  ":" =>  Token::Colon,
  "=" =>  Token::Equal,
  "@" =>  Token::At,
  "->" =>  Token::Rarrow,
  "|" =>  Token::Vbar,
  "(" =>  Token::Lpar,
  ")" =>  Token::Rpar,
  "[" =>  Token::Lsqb,
  "]" =>  Token::Rsqb,
  "?" =>  Token::Question,
  "=>" =>  Token::REqArrow,
  "and" =>  Token::And,
  "or" =>  Token::Or,
  "not" =>  Token::Not,
  "==" =>  Token::DoubleEq,
  "!=" =>  Token::NotEq,
  "<" =>  Token::Less,
  "<=" =>  Token::LessEq,
  ">" =>  Token::Greater,
  ">=" =>  Token::GreaterEq,
  "in" =>  Token::In,
  "+" =>  Token::Plus,
  "-" =>  Token::Minus,
  "*" =>  Token::Star,
  "/" =>  Token::Slash,
  "%" =>  Token::Percent,
};
