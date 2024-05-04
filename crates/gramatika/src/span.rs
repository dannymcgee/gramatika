use std::{cmp::Ordering, fmt};

use crate::DebugLisp;

/// A simple representation of the location of some substring within a larger string.
/// Primarily used by [`SpannedError`] to provide user-friendly error formatting.
///
/// [`SpannedError`]: crate::error::SpannedError
#[derive(Clone, Copy, Default, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Span {
	pub start: Position,
	pub end: Position,
}

/// Represents a cursor position within a string. Line and character offsets are
/// zero-indexed in the internal representation, but lines will be printed as 1-indexed by
/// [`SpannedError`] for consistency with IDEs. Character offsets are relative to the
/// current line.
///
/// [`SpannedError`]: crate::error::SpannedError
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Position {
	pub line: usize,
	pub character: usize,
}

impl Ord for Position {
	fn cmp(&self, other: &Self) -> Ordering {
		if self.line == other.line {
			self.character.cmp(&other.character)
		} else {
			self.line.cmp(&other.line)
		}
	}
}

impl PartialOrd for Position {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

pub trait Spanned {
	fn span(&self) -> Span;
}

#[doc(hidden)]
#[macro_export]
#[deprecated(
	since = "0.5.0",
	note = "\n\
	The `span!` macro signature with three dots and zero-based indices has \
	been deprecated.\n\n\
	Use the new signature instead, with two dots and one-based indices:\n\
	- Old: `span!(0:0...0:4)`\n\
	- New: `span!(1:1..1:5)`\n\n\
	See here for more info: https://github.com/dannymcgee/gramatika/pull/5"
)]
macro_rules! __span_deprecated {
	($start_line:literal:$start_char:literal...$end_line:literal:$end_char:literal) => {
		$crate::Span::new(($start_line, $start_char), ($end_line, $end_char))
	};
}

#[macro_export]
macro_rules! span {
	($start_line:literal : $start_char:literal .. $end_line:literal : $end_char:literal) => {
		::gramatika::Span::new(
			($start_line - 1, $start_char - 1),
			($end_line - 1, $end_char - 1),
		)
	};
	($start_line:literal:$start_char:literal...$end_line:literal:$end_char:literal) => {
		$crate::__span_deprecated!($start_line:$start_char...$end_line:$end_char)
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

	pub fn through(self, other: Span) -> Span {
		Span {
			start: self.start,
			end: other.end,
		}
	}

	pub fn contains(&self, other: Span) -> bool {
		self.start <= other.start && self.end >= other.end
	}
}

impl fmt::Debug for Span {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}..{:?}", self.start, self.end)
	}
}

impl DebugLisp for Span {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _: usize) -> fmt::Result {
		fmt::Debug::fmt(self, f)
	}
}

impl fmt::Debug for Position {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}:{}", self.line + 1, self.character + 1)
	}
}
