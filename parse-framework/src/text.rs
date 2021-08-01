use std::fmt;

#[derive(Clone, Copy, Default, PartialEq)]
pub struct Span {
	pub start: Position,
	pub end: Position,
}

impl fmt::Debug for Span {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}...{:?}", self.start, self.end)
	}
}

#[derive(Clone, Copy, Default, PartialEq)]
pub struct Position {
	pub line: usize,
	pub character: usize,
}

impl Span {
	pub fn new(start: (usize, usize), end: (usize, usize)) -> Self {
		Self {
			start: Position {
				line: start.0,
				character: start.1,
			},
			end: Position {
				line: end.0,
				character: end.1,
			},
		}
	}
}

impl fmt::Debug for Position {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}:{}", self.line, self.character)
	}
}

pub trait Token {
	fn span(&self) -> Span;
	fn lexeme(&self) -> &str;
}
