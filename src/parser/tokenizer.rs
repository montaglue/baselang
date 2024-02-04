use std::str::SplitWhitespace;

use crate::utils::span::Span;

pub struct Tokenizer<'s, T> {
    pub iter: T,
    pub source: &'s str,
    pos: usize,
    close_bracket: bool,
}

fn addr_of(s: &str) -> usize {
    s.as_ptr() as usize
}

fn code_iterator(begin: usize, s: &str) -> impl Iterator<Item = (usize, &str)> {
    s.split_whitespace()
        .flat_map(|s| s.split_inclusive('('))
        .flat_map(|s| s.split_inclusive(')'))
        .flat_map(|s| s.split_inclusive('|'))
        .map(move |s| (addr_of(s) - begin, s))
}

fn string_literal_iterator(begin: usize, s: &str) -> impl Iterator<Item = (usize, &str)> {
    [(addr_of(s) - begin, s)].into_iter()
}

fn region_iterator<'s>(
    begin: usize,
    len: usize,
    s: &'s str,
) -> impl Iterator<Item = (usize, usize)> + 's {
    let mut state = false;
    s.split_inclusive('"').filter_map(move |s| {
        let mut start = addr_of(s) - begin;
        let mut end = start + s.len();
        if state {
            start -= 1;
        } else if end != len {
            end -= 1
        }
        state = !state;
        if start < end {
            Some((start, end))
        } else {
            None
        }
    })
}

enum EitherIter<A, B> {
    Left(A),
    Right(B),
}

impl<I, A: Iterator<Item = I>, B: Iterator<Item = I>> Iterator for EitherIter<A, B> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            EitherIter::Left(a) => a.next(),
            EitherIter::Right(b) => b.next(),
        }
    }
}

impl<'s> Tokenizer<'s, SplitWhitespace<'_>> {
    pub fn new(source: &'s str) -> Tokenizer<impl Iterator<Item = (usize, &'s str)>> {
        let begin = addr_of(source);
        let len = source.len();
        let mut state = true;
        let iter = region_iterator(begin, len, source).flat_map(move |(start, end)| {
            state = !state;
            if state {
                EitherIter::Left(string_literal_iterator(begin, &source[start..end]))
            } else {
                EitherIter::Right(code_iterator(begin, &source[start..end]))
            }
        });
        // source
        // .split_whitespace()
        // .flat_map(|s| s.split_inclusive('('))
        // .flat_map(|s| s.split_inclusive(')'))
        // .flat_map(|s| s.split_inclusive('|'))
        // .map(move |s| (addr_of(s) - begin, s));
        Tokenizer {
            iter,
            source,
            pos: 0,
            close_bracket: false,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenType,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Integer(i64),
    Float(f64),
    Ident,
    OpenParen,
    Border,
    CloseParen,
}

impl<'s, T> Iterator for Tokenizer<'s, T>
where
    T: Iterator<Item = (usize, &'s str)>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.close_bracket {
            self.close_bracket = false;

            let start = self.pos;
            self.pos += 1;
            let end = self.pos;

            return Some(Token {
                span: Span(start, end),
                kind: TokenType::CloseParen,
            });
        }
        let (index, mut s) = self.iter.next()?;
        self.pos = index;
        if s.ends_with(')') && s.len() > 1 {
            s = &s[0..s.len() - 1];
            self.close_bracket = true;
        }

        let begin = self.pos;
        self.pos += s.len();
        let end = self.pos;
        let mut kind = match s {
            "(" => Some(TokenType::OpenParen),
            ")" => Some(TokenType::CloseParen),
            "|" => Some(TokenType::Border),
            _ => None,
        };

        if kind.is_none() {
            let num: Result<i64, _> = s.parse();
            kind = num.ok().map(TokenType::Integer);
        }

        let kind = kind.unwrap_or(TokenType::Ident);

        Some(Token {
            span: Span(begin, end),
            kind,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_tokenizer() {
        let source = "a\"a\"\"abacaba\"a\"a\"";
        let v = source.split_inclusive('"').collect::<Vec<_>>();
        println!("{:?}", v);
    }
}
