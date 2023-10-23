use crate::open_scenario::osc2::runner::lex::lexer::Spanned;

use super::{errors::ParseError, scenario::Scenario};

pub fn parse_tokens(spans: Vec<Spanned>) -> Result<Scenario, ParseError> {
    Scenario::parse_scenario(Spans::new(spans))
}

pub struct Spans {
    source: Vec<Spanned>,
}

impl Spans {
    pub fn new(source: Vec<Spanned>) -> Self {
        Self { source }
    }

    pub fn iter(&self) -> SpanIterator<'_> {
        SpanIterator::new(self)
    }
}

pub struct SpanIterator<'a> {
    spans: &'a Spans,
    current_idx: usize,
}

impl<'a> SpanIterator<'a> {
    fn new(spans: &'a Spans) -> SpanIterator<'_> {
        SpanIterator {
            spans,
            current_idx: 0,
        }
    }

    pub fn peek(&self, idx: usize) -> Option<&'a Spanned> {
        if self.current_idx + idx >= self.spans.source.len() {
            None
        } else {
            Some(&self.spans.source[self.current_idx + idx])
        }
    }
}

impl<'a> Iterator for SpanIterator<'a> {
    type Item = &'a Spanned;
    fn next(&mut self) -> Option<Self::Item> {
        if self.current_idx >= self.spans.source.len() {
            None
        } else {
            self.current_idx += 1;
            Some(&self.spans.source[self.current_idx - 1])
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::open_scenario::osc2::runner::lex::location::Location;
    use crate::open_scenario::osc2::runner::lex::token::Token;

    use super::*;

    #[test]
    fn test_token_iteration_and_peek() {
        let loc = Location::new(0, 0);
        let spans = Spans::new(vec![
            Spanned {
                token: Token::Action,
                start_loc: loc,
                end_loc: loc,
            },
            Spanned {
                token: Token::Bool,
                start_loc: loc,
                end_loc: loc,
            },
            Spanned {
                token: Token::As,
                start_loc: loc,
                end_loc: loc,
            },
            Spanned {
                token: Token::With,
                start_loc: loc,
                end_loc: loc,
            },
        ]);
        let mut span_iter = spans.iter();

        assert_eq!(
            span_iter.peek(0),
            Some(&Spanned {
                token: Token::Action,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(
            span_iter.peek(1),
            Some(&Spanned {
                token: Token::Bool,
                start_loc: loc,
                end_loc: loc
            })
        );

        assert_eq!(
            span_iter.next(),
            Some(&Spanned {
                token: Token::Action,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(
            span_iter.peek(0),
            Some(&Spanned {
                token: Token::Bool,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(
            span_iter.peek(1),
            Some(&Spanned {
                token: Token::As,
                start_loc: loc,
                end_loc: loc
            })
        );

        assert_eq!(
            span_iter.next(),
            Some(&Spanned {
                token: Token::Bool,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(
            span_iter.peek(0),
            Some(&Spanned {
                token: Token::As,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(
            span_iter.peek(1),
            Some(&Spanned {
                token: Token::With,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(span_iter.peek(2), None);

        assert_eq!(
            span_iter.next(),
            Some(&Spanned {
                token: Token::As,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(
            span_iter.peek(0),
            Some(&Spanned {
                token: Token::With,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(span_iter.peek(1), None);

        assert_eq!(
            span_iter.next(),
            Some(&Spanned {
                token: Token::With,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(span_iter.peek(0), None);
        assert_eq!(span_iter.peek(1), None);

        assert_eq!(span_iter.next(), None);
        assert_eq!(span_iter.peek(0), None);
    }

    #[test]
    fn test_token_iteration_and_peek_empty() {
        let empty_spans = Spans::new(vec![]);
        let mut span_iter = empty_spans.iter();

        assert_eq!(span_iter.next(), None);
        assert_eq!(span_iter.peek(0), None);
        assert_eq!(span_iter.peek(1), None);

        let loc = Location::new(0, 0);
        let one_span = Spans::new(vec![Spanned {
            token: Token::Action,
            start_loc: loc,
            end_loc: loc,
        }]);

        let mut span_iter = one_span.iter();

        assert_eq!(
            span_iter.peek(0),
            Some(&Spanned {
                token: Token::Action,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(span_iter.peek(1), None);

        assert_eq!(
            span_iter.next(),
            Some(&Spanned {
                token: Token::Action,
                start_loc: loc,
                end_loc: loc
            })
        );
        assert_eq!(span_iter.peek(0), None);
        assert_eq!(span_iter.peek(1), None);
    }
}
