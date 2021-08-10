use std::fmt;

#[derive(Clone, Copy, Default, PartialEq)]
pub struct Span {
	pub start: Position,
	pub end: Position,
}

#[derive(Clone, Copy, Default, PartialEq)]
pub struct Position {
	pub line: usize,
	pub character: usize,
}

#[macro_export]
macro_rules! span {
	($start_line:literal:$start_char:literal...$end_line:literal:$end_char:literal) => {
		::gramatika::Span::new(($start_line, $start_char), ($end_line, $end_char))
	};
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

impl fmt::Debug for Span {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}...{:?}", self.start, self.end)
	}
}

impl DebugLisp for Span {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _: usize) -> fmt::Result {
		fmt::Debug::fmt(self, f)
	}
}

impl fmt::Debug for Position {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}:{}", self.line, self.character)
	}
}
