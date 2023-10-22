use core::ops::Index;
use std::{cmp::Ordering, slice::SliceIndex};

use crate::open_scenario::osc2::parser::consts::KEYWORD_MAP;

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
    ) -> Result<Ordering, LexicalError> {
        let self_tabs = self.tabs * 8;
        let other_tabs = other.tabs * 8;
        if (self_tabs != 0 && self.spaces % self_tabs != 0)
            || (other_tabs != 0 && other.spaces % other_tabs != 0)
        {
            return Err(LexicalError {
                error: LexicalErrorType::IndentationError,
                location: *location,
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
pub type Spanned = (Location, Token, Location);

pub type LexResult = Result<Spanned, LexicalError>;

pub struct Lexer<T: Iterator<Item = char>> {
    window: CharWindow<T, 3>,

    at_begin_of_line: bool,
    nesting: usize, // # of paranthesis
    indentations: Indentations,
    pending: Vec<Spanned>,
    location: Location,
}

#[inline]
pub fn make_tokenizer(source: &str) -> impl Iterator<Item = LexResult> + '_ {
    make_tokenizer_located(source, Location::new(0, 0))
}

pub fn make_tokenizer_located(
    source: &str,
    start_location: Location,
) -> impl Iterator<Item = LexResult> + '_ {
    let nlh = NewLineHandler::new(source.chars());
    Lexer::new(nlh, start_location)
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
    pub fn new(input: T, start: Location) -> Self {
        let mut lxr = Lexer {
            window: CharWindow::new(input),
            at_begin_of_line: true,
            nesting: 0,
            indentations: Indentations::default(),
            pending: Vec::new(),
            location: start,
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
        let ordering = indentation_level.compare_strict(current_indentation, &self.pos())?;
        match ordering {
            Ordering::Equal => {
                // Same indentation
            }
            Ordering::Greater => {
                // new indentation
                self.indentations.push(indentation_level);
                let tok_pos = self.pos();
                self.emit((tok_pos, Token::Indent, tok_pos));
            }
            Ordering::Less => {
                // One or more dedentation
                // Pop other levels until matching level found.
                loop {
                    let current_indentation = self.indentations.current();
                    let ordering =
                        indentation_level.compare_strict(current_indentation, &self.location)?;
                    match ordering {
                        Ordering::Less => {
                            self.indentations.pop();
                            let tok_pos = self.pos();
                            self.emit((tok_pos, Token::Dedent, tok_pos));
                        }
                        Ordering::Equal => {
                            // matching indentation found
                            break;
                        }
                        Ordering::Greater => {
                            return Err(LexicalError {
                                error: LexicalErrorType::IndentationError,
                                location: self.pos(),
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
            Ok((start_pos, token.clone(), end_pos))
        } else {
            Ok((start_pos, Token::Identifier { identifier: name }, end_pos))
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
                });
            }

            // Insert one newline if we are at the middle of a line
            if !self.at_begin_of_line {
                self.at_begin_of_line = true;
                self.emit((start_pos, Token::Newline, start_pos));
            }

            // Then flush remaining indentation
            while !self.indentations.is_empty() {
                self.indentations.pop();
                self.emit((start_pos, Token::Dedent, start_pos));
            }

            self.emit((start_pos, Token::EndOfFile, start_pos));
        }
        Ok(())
    }

    fn consume_character(&mut self, c: char) -> Result<(), LexicalError> {
        match c {
            ':' => {
                self.consume_single_char(Token::Colon);
            }
            '\n' => {
                let start_pos = self.pos();
                self.next_char();
                let end_pos = self.pos();
                if self.nesting == 0 {
                    self.at_begin_of_line = true;
                    self.emit((start_pos, Token::Newline, end_pos));
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
                        self.emit((start_pos, Token::Rarrow, end_pos));
                    }
                    _ => todo!(),
                }
            }
            _ => {
                todo!();
            }
        }
        Ok(())
    }

    fn consume_single_char(&mut self, token: Token) {
        let start_pos = self.pos();
        self.next_char()
            .expect(format!("expect one char at {:?}", start_pos).as_str());
        let end_pos = self.pos();
        self.emit((start_pos, token, end_pos));
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
            Ok((_, Token::EndOfFile, _)) => None,
            r => Some(r),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::Chars;

    use super::*;

    pub fn lex_source(source: &str) -> Vec<Token> {
        let lexer = make_tokenizer(source);
        lexer.map(|x| x.unwrap().1).collect()
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
}
