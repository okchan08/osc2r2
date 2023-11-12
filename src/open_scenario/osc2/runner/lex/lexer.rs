use core::ops::Index;
use std::{cmp::Ordering, slice::SliceIndex};

use bevy::utils::tracing::Span;

use super::consts::KEYWORD_MAP;

use super::{
    consts::{ASCII_LOWER, ASCII_UPPER},
    errors::{LexicalError, LexicalErrorType},
    location::Location,
    token::Token,
};

#[derive(Default, Debug)]
struct IndentationLevel {
    tabs: usize,
    spaces: usize,
}

impl IndentationLevel {
    // 1 tab is equivalent to 8 whitespaces.
    fn compare_strict(
        &self,
        other: &IndentationLevel,
        location: &Location,
        filename: &str,
    ) -> Result<Ordering, LexicalError> {
        let self_tabs = self.tabs * 8;
        let other_tabs = other.tabs * 8;
        if (self_tabs != 0 && self.spaces % self_tabs != 0)
            || (other_tabs != 0 && other.spaces % other_tabs != 0)
        {
            return Err(LexicalError {
                error: LexicalErrorType::IndentationError,
                location: *location,
                filename: filename.to_owned(),
            });
        }

        Ok((self_tabs + self.spaces).cmp(&(other_tabs + other.spaces)))
    }
}

#[derive(Debug)]
struct Indentations {
    indent_stack: Vec<IndentationLevel>,
}

impl Default for Indentations {
    fn default() -> Self {
        Self {
            indent_stack: vec![Default::default()],
        }
    }
}

impl Indentations {
    fn is_empty(&self) -> bool {
        self.indent_stack.len() == 1
    }

    fn push(&mut self, indent: IndentationLevel) {
        self.indent_stack.push(indent);
    }

    fn pop(&mut self) -> Option<IndentationLevel> {
        if self.is_empty() {
            return None;
        }
        self.indent_stack.pop()
    }

    fn current(&self) -> &IndentationLevel {
        self.indent_stack
            .last()
            .expect("At least one indentation should exist.")
    }
}

struct CharWindow<T: Iterator<Item = char>, const N: usize> {
    source: T,
    window: [Option<char>; N],
}

impl<T, const N: usize> CharWindow<T, N>
where
    T: Iterator<Item = char>,
{
    fn new(source: T) -> Self {
        Self {
            source,
            window: [None; N],
        }
    }

    fn slide(&mut self) -> Option<char> {
        self.window.rotate_left(1);
        let next = self.source.next();
        *self.window.last_mut().expect("window never be empty") = next;
        next
    }

    fn change_first(&mut self, ch: char) {
        *self.window.first_mut().expect("window never be empty") = Some(ch);
    }
}

impl<T, const N: usize, Idx> Index<Idx> for CharWindow<T, N>
where
    T: Iterator<Item = char>,
    Idx: SliceIndex<[Option<char>], Output = Option<char>>,
{
    type Output = Option<char>;
    fn index(&self, index: Idx) -> &Self::Output {
        self.window.index(index)
    }
}

// represents token location.
// (first location of token in file, found token type, last location of token in file)
#[derive(Debug, PartialEq)]
pub struct Spanned {
    pub token: Token,
    pub start_loc: Location,
    pub end_loc: Location,
}

pub type LexResult = Result<Spanned, LexicalError>;

pub struct Lexer<T: Iterator<Item = char>> {
    window: CharWindow<T, 3>,

    at_begin_of_line: bool,
    nesting: usize, // # of paranthesis
    indentations: Indentations,
    pending: Vec<Spanned>,
    location: Location,
    filename: String,
}

#[inline]
pub fn make_tokenizer(source: &str, filename: String) -> impl Iterator<Item = LexResult> + '_ {
    make_tokenizer_located(source, Location::new(0, 0), filename)
}

pub fn make_tokenizer_located(
    source: &str,
    start_location: Location,
    filename: String,
) -> impl Iterator<Item = LexResult> + '_ {
    let nlh = NewLineHandler::new(source.chars());
    Lexer::new(nlh, start_location, filename)
}

// The newline handler is an iterator which collapses different newline
// types into \n always.
pub struct NewLineHandler<T: Iterator<Item = char>> {
    window: CharWindow<T, 2>,
}

impl<T> NewLineHandler<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(source: T) -> Self {
        let mut nlh = NewLineHandler {
            window: CharWindow::new(source),
        };
        // call CharWindow::shift twice to populate the inner char array.
        // note that the inner CharWindow has length of 2.
        nlh.shift();
        nlh.shift();
        nlh
    }

    // move to next character.
    fn shift(&mut self) -> Option<char> {
        let result = self.window[0];
        self.window.slide();
        result
    }
}

impl<T> Iterator for NewLineHandler<T>
where
    T: Iterator<Item = char>,
{
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        // Collapse \r\n into \n
        loop {
            match (self.window[0], self.window[1]) {
                (Some('\r'), Some('\n')) => {
                    // Windows EOL into \n
                    self.shift();
                }
                (Some('\r'), _) => {
                    // MAC EOL into \n
                    self.window.change_first('\n');
                }
                _ => break,
            }
        }
        // At the end of loop the first char is '\n'.
        // shift one char so that the handler points to next line.
        self.shift()
    }
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(input: T, start: Location, filename: String) -> Self {
        let mut lxr = Lexer {
            window: CharWindow::new(input),
            at_begin_of_line: true,
            nesting: 0,
            indentations: Indentations::default(),
            pending: Vec::new(),
            location: start,
            filename,
        };

        // Populate inner window by calling slide 3 times.
        // Note that the this window has length = 3.
        lxr.window.slide();
        lxr.window.slide();
        lxr.window.slide();

        // Start at top row (=1) left column (=1)
        lxr.location.reset();
        lxr
    }

    // entry point of the lexer, would be called via Iterator::next method.
    fn inner_next(&mut self) -> LexResult {
        while self.pending.is_empty() {
            if self.at_begin_of_line {
                self.handle_indentation()?;
            }
            self.consume_normal()?;
        }
        Ok(self.pending.remove(0))
    }

    fn handle_indentation(&mut self) -> Result<(), LexicalError> {
        let indentation_level = self.eat_indentation()?;

        if self.nesting != 0 {
            return Ok(());
        }

        // Determine indent or dedent
        let current_indentation = self.indentations.current();
        let ordering =
            indentation_level.compare_strict(current_indentation, &self.pos(), &self.filename)?;
        match ordering {
            Ordering::Equal => {
                // Same indentation
            }
            Ordering::Greater => {
                // new indentation
                self.indentations.push(indentation_level);
                let tok_pos = self.pos();
                self.emit(Spanned {
                    start_loc: tok_pos,
                    token: Token::Indent,
                    end_loc: tok_pos,
                });
            }
            Ordering::Less => {
                // One or more dedentation
                // Pop other levels until matching level found.
                loop {
                    let current_indentation = self.indentations.current();
                    let ordering = indentation_level.compare_strict(
                        current_indentation,
                        &self.location,
                        &self.filename,
                    )?;
                    match ordering {
                        Ordering::Less => {
                            self.indentations.pop();
                            let tok_pos = self.pos();
                            self.emit(Spanned {
                                token: Token::Dedent,
                                start_loc: tok_pos,
                                end_loc: tok_pos,
                            });
                        }
                        Ordering::Equal => {
                            // matching indentation found
                            break;
                        }
                        Ordering::Greater => {
                            return Err(LexicalError {
                                error: LexicalErrorType::IndentationError,
                                location: self.pos(),
                                filename: self.filename.clone(),
                            })
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Given we are at the start of a line, count the number of spaces and/or tabs until the first character.
    fn eat_indentation(&mut self) -> Result<IndentationLevel, LexicalError> {
        let mut spaces = 0;
        let mut tabs = 0;
        loop {
            match self.window[0] {
                Some(' ') => {
                    self.next_char();
                    spaces += 1;
                }
                Some('\t') => {
                    if spaces != 0 {
                        // mixure of tab and space for indentation.
                        return Err(LexicalError {
                            error: LexicalErrorType::TabsAfterSpaces,
                            location: self.pos(),
                            filename: self.filename.clone(),
                        });
                    }
                    tabs += 1;
                }
                Some('\n') => {
                    // Empty line!
                    self.next_char();
                    spaces = 0;
                    tabs = 0;
                }
                None => {
                    spaces = 0;
                    tabs = 0;
                    break;
                }
                _ => {
                    self.at_begin_of_line = false;
                    break;
                }
            }
        }
        Ok(IndentationLevel { tabs, spaces })
    }

    /// Helper function to go to next character.
    fn next_char(&mut self) -> Option<char> {
        let c = self.window[0];
        self.window.slide();
        if c == Some('\n') {
            self.location.newline();
        } else {
            self.location.go_right();
        }
        c
    }

    fn is_identifier_start(&self, c: char) -> bool {
        ASCII_LOWER.contains(&c) || ASCII_UPPER.contains(&c)
    }

    fn is_identifier_continuation(&self) -> bool {
        if let Some(c) = self.window[0] {
            match c {
                '_' | '0'..='9' => true,
                c => ASCII_LOWER.contains(&c) || ASCII_UPPER.contains(&c),
            }
        } else {
            false
        }
    }

    fn lex_identifier(&mut self) -> LexResult {
        let mut name = String::new();
        let start_pos = self.pos();

        name.push(self.next_char().expect("unnexpected EOF"));
        while self.is_identifier_continuation() {
            name.push(self.next_char().expect("unnexpected EOF"))
        }

        let end_pos = self.pos();

        if let Some(token) = KEYWORD_MAP.get(name.as_str()) {
            Ok(Spanned {
                token: token.clone(),
                start_loc: start_pos,
                end_loc: end_pos,
            })
        } else {
            Ok(Spanned {
                start_loc: start_pos,
                token: Token::Identifier { identifier: name },
                end_loc: end_pos,
            })
        }
    }

    fn consume_normal(&mut self) -> Result<(), LexicalError> {
        if let Some(c) = self.window[0] {
            if self.is_identifier_start(c) {
                let identifier = self.lex_identifier()?;
                self.emit(identifier);
            } else {
                self.consume_character(c)?;
            }
        } else {
            // reached end of file
            let start_pos = self.pos();

            // Raise an error if there's remaining unclosed paranthesis
            if self.nesting > 0 {
                return Err(LexicalError {
                    error: LexicalErrorType::EOF,
                    location: start_pos,
                    filename: self.filename.clone(),
                });
            }

            // TODO is this necessary?
            // Insert one newline if we are at the middle of a line
            //if !self.at_begin_of_line {
            //    self.at_begin_of_line = true;
            //    self.emit(Spanned {
            //        token: Token::Newline,
            //        start_loc: start_pos,
            //        end_loc: start_pos,
            //    });
            //}

            // Then flush remaining indentation
            while !self.indentations.is_empty() {
                self.indentations.pop();
                self.emit(Spanned {
                    token: Token::Dedent,
                    start_loc: start_pos,
                    end_loc: start_pos,
                });
            }

            self.emit(Spanned {
                token: Token::EndOfFile,
                start_loc: start_pos,
                end_loc: start_pos,
            });
        }
        Ok(())
    }

    fn consume_character(&mut self, c: char) -> Result<(), LexicalError> {
        match c {
            '0'..='9' => {
                let number = self.consume_number()?;
                self.emit(number);
            }
            ':' => {
                self.consume_single_char(Token::Colon);
            }
            '\n' => {
                let start_pos = self.pos();
                self.next_char();
                let end_pos = self.pos();
                if self.nesting == 0 {
                    self.at_begin_of_line = true;
                    self.emit(Spanned {
                        token: Token::Newline,
                        start_loc: start_pos,
                        end_loc: end_pos,
                    });
                }
            }
            ' ' | '\t' | '\x0C' => {
                // Skip whitespaces
                self.next_char();
                while let Some(' ' | '\t' | '\x0C') = self.window[0] {
                    self.next_char();
                }
            }
            ',' => {
                self.consume_single_char(Token::Comma);
            }
            '(' => {
                self.consume_single_char(Token::Lpar);
            }
            ')' => {
                self.consume_single_char(Token::Rpar);
            }
            '-' => {
                let start_pos = self.pos();
                self.next_char();
                match self.window[0] {
                    Some('>') => {
                        self.next_char();
                        let end_pos = self.pos();
                        self.emit(Spanned {
                            token: Token::Rarrow,
                            start_loc: start_pos,
                            end_loc: end_pos,
                        });
                    }
                    _ => {
                        let end_pos = self.pos();
                        self.emit(Spanned {
                            token: Token::Minus,
                            start_loc: start_pos,
                            end_loc: end_pos,
                        });
                    }
                }
            }
            '#' => {
                // comment until the end of line.
                self.next_char();
                while let Some(c) = self.window[0] {
                    self.next_char();
                    if c == '\n' {
                        break;
                    }
                }
            }
            '=' => {
                let start_pos = self.pos();
                self.next_char();
                match self.window[0] {
                    Some('=') => {
                        self.next_char();
                        let end_pos = self.pos();
                        self.emit(Spanned {
                            token: Token::DoubleEq,
                            start_loc: start_pos,
                            end_loc: end_pos,
                        });
                    }
                    Some(_) => {
                        let end_pos = self.pos();
                        self.emit(Spanned {
                            token: Token::Equal,
                            start_loc: start_pos,
                            end_loc: end_pos,
                        });
                    }
                    None => {
                        return Err(LexicalError {
                            error: LexicalErrorType::EOF,
                            location: start_pos,
                            filename: self.filename.to_owned(),
                        });
                    }
                }
            }
            '.' => {
                let start_pos = self.pos();
                self.next_char();
                match self.window[0] {
                    Some('.') => {
                        self.next_char();
                        let end_pos = self.pos();
                        self.emit(Spanned {
                            token: Token::DoublePeriod,
                            start_loc: start_pos,
                            end_loc: end_pos,
                        });
                    }
                    Some(_) => {
                        let end_pos = self.pos();
                        self.emit(Spanned {
                            token: Token::Period,
                            start_loc: start_pos,
                            end_loc: end_pos,
                        });
                    }
                    None => {
                        return Err(LexicalError {
                            error: LexicalErrorType::EOF,
                            location: start_pos,
                            filename: self.filename.to_owned(),
                        });
                    }
                }
            }
            '"' => {
                self.consume_single_char(Token::DoubleQuotation);
            }
            '*' => {
                self.consume_single_char(Token::Star);
            }
            '/' => {
                self.consume_single_char(Token::Slash);
            }
            '%' => {
                self.consume_single_char(Token::Percent);
            }
            '+' => {
                self.consume_single_char(Token::Plus);
            }
            '!' => {
                let start_pos = self.pos();
                self.next_char();
                match self.window[0] {
                    Some('=') => {
                        self.next_char();
                        let end_pos = self.pos();
                        self.emit(Spanned {
                            token: Token::NotEq,
                            start_loc: start_pos,
                            end_loc: end_pos,
                        })
                    }
                    Some(_) => self.emit(Spanned {
                        token: Token::Exclamation,
                        start_loc: start_pos,
                        end_loc: start_pos,
                    }),
                    None => {
                        return Err(LexicalError {
                            error: LexicalErrorType::EOF,
                            location: start_pos,
                            filename: self.filename.to_owned(),
                        });
                    }
                }
            }
            '<' => {
                let start_pos = self.pos();
                self.next_char();
                match self.window[0] {
                    Some('=') => {
                        self.next_char();
                        let end_pos = self.pos();
                        self.emit(Spanned {
                            token: Token::LessEq,
                            start_loc: start_pos,
                            end_loc: end_pos,
                        });
                    }
                    Some(_) => self.emit(Spanned {
                        token: Token::Less,
                        start_loc: start_pos,
                        end_loc: start_pos,
                    }),
                    None => {
                        return Err(LexicalError {
                            error: LexicalErrorType::EOF,
                            location: start_pos,
                            filename: self.filename.to_owned(),
                        });
                    }
                }
            }
            _ => {
                return Err(LexicalError {
                    error: LexicalErrorType::NotSupportedYet,
                    location: self.pos(),
                    filename: self.filename.to_owned(),
                });
            }
        }
        Ok(())
    }

    // Test if a digit is of a certain radix.
    fn is_digit_of_radix(c: Option<char>, radix: u32) -> bool {
        match radix {
            2 => matches!(c, Some('0'..='1')),
            8 => matches!(c, Some('0'..='7')),
            10 => matches!(c, Some('0'..='9')),
            16 => matches!(c, Some('0'..='9') | Some('a'..='f') | Some('A'..='F')),
            other => unimplemented!("Radix not implemented: {}", other),
        }
    }

    // Consume a single character with the given radix.
    fn take_number(&mut self, radix: u32) -> Option<char> {
        let take_char = Lexer::<T>::is_digit_of_radix(self.window[0], radix);

        if take_char {
            Some(self.next_char()?)
        } else {
            None
        }
    }

    // Consume a sequence of numbers with the given radix,
    // the digits can be decorated with underscores
    // like this: '1_2_3_4' == '1234'
    fn radix_run(&mut self, radix: u32) -> String {
        let mut value_text = String::new();

        loop {
            if let Some(c) = self.take_number(radix) {
                value_text.push(c);
            } else if self.window[0] == Some('_')
                && Lexer::<T>::is_digit_of_radix(self.window[1], radix)
            {
                self.next_char();
            } else {
                break;
            }
        }
        value_text
    }

    fn consume_number(&mut self) -> LexResult {
        let start_pos = self.pos();

        let mut value_text = self.radix_run(10);
        if self.window[0] == Some('.') {
            // take '.' in 123.456
            value_text.push(self.next_char().unwrap());
            value_text.push_str(&self.radix_run(10));
            let end_pos = self.pos();
            Ok(Spanned {
                token: Token::FloatNumber(value_text.parse().unwrap()),
                start_loc: start_pos,
                end_loc: end_pos,
            })
        } else {
            let end_pos = self.pos();
            Ok(Spanned {
                token: Token::IntNumber(value_text.parse().unwrap()),
                start_loc: start_pos,
                end_loc: end_pos,
            })
        }
    }

    fn consume_single_char(&mut self, token: Token) {
        let start_pos = self.pos();
        self.next_char()
            .unwrap_or_else(|| panic!("expect one char in {}:{:?}", self.filename, start_pos));
        let end_pos = self.pos();
        self.emit(Spanned {
            token,
            start_loc: start_pos,
            end_loc: end_pos,
        });
    }

    fn pos(&self) -> Location {
        self.location
    }

    fn emit(&mut self, spanned: Spanned) {
        self.pending.push(spanned);
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner_next();
        match token {
            Ok(Spanned {
                token: Token::EndOfFile,
                ..
            }) => None,
            r => Some(r),
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::str::Chars;

    use super::*;

    pub fn lex_source(source: &str) -> Vec<Token> {
        let lexer = make_tokenizer(source, "".to_string());
        lexer.map(|x| x.unwrap().token).collect()
    }

    #[test]
    fn test_char_window() {
        let mut cw = CharWindow::<Chars, 3>::new("char window".chars());
        assert_eq!(cw[0], None);
        assert_eq!(cw[1], None);
        assert_eq!(cw[2], None);

        assert_eq!(cw.slide(), Some('c'));
        assert_eq!(cw.slide(), Some('h'));
        assert_eq!(cw.slide(), Some('a'));

        assert_eq!(cw[0], Some('c'));
        assert_eq!(cw[1], Some('h'));
        assert_eq!(cw[2], Some('a'));
    }

    #[test]
    fn test_new_line_handler() {
        let mut nlh = NewLineHandler::new("\r\nsecond line".chars());
        assert_eq!(nlh.next(), Some('\n'));
    }

    #[test]
    fn test_lexer() {
        let source1 = "actor sample_actor:\n";

        assert_eq!(
            lex_source(source1),
            vec![
                Token::Actor,
                Token::Identifier {
                    identifier: "sample_actor".to_string()
                },
                Token::Colon,
                Token::Newline,
            ]
        );

        let source2 = "
actor sample_actor_with_method:
    def sample_function() -> output_type is undefined
";
        assert_eq!(
            lex_source(source2),
            vec![
                Token::Actor,
                Token::Identifier {
                    identifier: "sample_actor_with_method".to_string()
                },
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Def,
                Token::Identifier {
                    identifier: "sample_function".to_string()
                },
                Token::Lpar,
                Token::Rpar,
                Token::Rarrow,
                Token::Identifier {
                    identifier: "output_type".to_string()
                },
                Token::Is,
                Token::Undefined,
                Token::Newline,
                Token::Dedent,
            ]
        )
    }

    #[test]
    fn test_number_lex() {
        let source = "12345 9.123 0.1234000 -123.4 -99";
        assert_eq!(
            lex_source(source),
            vec![
                Token::IntNumber(12345),
                Token::FloatNumber(9.123),
                Token::FloatNumber(0.1234),
                Token::Minus,
                Token::FloatNumber(123.4),
                Token::Minus,
                Token::IntNumber(99),
            ]
        );
    }

    #[test]
    fn test_long_input() {
        let source = "
struct road_info:
    road_id: int
    lane_id: int
    s: float

actor my_car:
    var road_info: road_info
    var speed: float

scenario my_scenario:
    do serial:
        emit start_ev

        my_car.speed
";
        let result = lex_source(source);
        assert_eq!(
            result,
            vec![
                Token::Struct,
                Token::Identifier {
                    identifier: "road_info".to_string()
                },
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier {
                    identifier: "road_id".to_string()
                },
                Token::Colon,
                Token::Int,
                Token::Newline,
                Token::Identifier {
                    identifier: "lane_id".to_string()
                },
                Token::Colon,
                Token::Int,
                Token::Newline,
                Token::Identifier {
                    identifier: "s".to_string()
                },
                Token::Colon,
                Token::Float,
                Token::Newline,
                Token::Dedent,
                Token::Actor,
                Token::Identifier {
                    identifier: "my_car".to_string()
                },
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Var,
                Token::Identifier {
                    identifier: "road_info".to_string()
                },
                Token::Colon,
                Token::Identifier {
                    identifier: "road_info".to_string()
                },
                Token::Newline,
                Token::Var,
                Token::Identifier {
                    identifier: "speed".to_string()
                },
                Token::Colon,
                Token::Float,
                Token::Newline,
                Token::Dedent,
                Token::Scenario,
                Token::Identifier {
                    identifier: "my_scenario".to_string()
                },
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Do,
                Token::Serial,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Emit,
                Token::Identifier {
                    identifier: "start_ev".to_string()
                },
                Token::Newline,
                Token::Identifier {
                    identifier: "my_car".to_string()
                },
                Token::Period,
                Token::Identifier {
                    identifier: "speed".to_string()
                },
                Token::Newline,
                Token::Dedent,
                Token::Dedent,
            ]
        )
    }

    #[test]
    fn test_ignore_comment() {
        let source = "
struct simple_struct:
    member1: int  # ignore comment
    

    member2: float
";
        let result = lex_source(source);
        assert_eq!(
            result,
            vec![
                Token::Struct,
                Token::Identifier {
                    identifier: "simple_struct".to_string()
                },
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier {
                    identifier: "member1".to_string()
                },
                Token::Colon,
                Token::Int,
                Token::Newline,
                Token::Identifier {
                    identifier: "member2".to_string(),
                },
                Token::Colon,
                Token::Float,
                Token::Newline,
                Token::Dedent
            ]
        );
    }
}
