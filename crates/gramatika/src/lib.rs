pub mod debug;
pub mod error;
pub mod lexer;
pub mod parse;
pub mod span;

use std::{ops::Range, rc::Rc};

pub use debug::*;
pub use error::*;
pub use lexer::*;
pub use parse::*;
pub use span::*;

#[cfg(feature = "macros")]
pub use gramatika_macro::*;
#[cfg(feature = "macros")]
pub use lazy_static::lazy_static;
#[cfg(feature = "macros")]
pub use regex::*;

pub struct _Match {
	text: Rc<str>,
	start: usize,
	end: usize,
}

impl _Match {
	#[inline]
	pub fn start(&self) -> usize {
		self.start
	}

	#[inline]
	pub fn end(&self) -> usize {
		self.end
	}

	#[inline]
	pub fn range(&self) -> Range<usize> {
		self.start..self.end
	}

	#[inline]
	pub fn as_str(&self) -> Rc<str> {
		self.text.as_ref()[self.range()].into()
	}

	#[inline]
	pub fn new(haystack: Rc<str>, original: regex::Match<'_>) -> Self {
		Self {
			text: haystack,
			start: original.start(),
			end: original.end(),
		}
	}
}
