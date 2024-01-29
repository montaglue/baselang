use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span(pub usize, pub usize);

impl Span {
    pub fn range(&self) -> Range<usize> {
        self.0..self.1
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FSpan(pub Span, pub String);
