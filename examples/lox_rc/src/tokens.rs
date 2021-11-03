#![allow(unused_macros, dead_code)]

use std::{fmt, rc::Rc};

use gramatika::{DebugLisp, Regex, Span, Spanned, Token as _, _Match, lazy_static};

#[derive(PartialEq)]
pub enum Token {
	// #[pattern = r"(and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while)\b"]
	Keyword(Rc<str>, Span),

	// #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(Rc<str>, Span),

	// #[pattern = r"[(){}]"]
	Brace(Rc<str>, Span),

	// #[pattern = "[,.;]"]
	Punct(Rc<str>, Span),

	// #[pattern = "[=!<>]=?"]
	// #[pattern = "[-+*/]"]
	Operator(Rc<str>, Span),

	// #[pattern = "[0-9]+"]
	NumLit(Rc<str>, Span),

	// #[pattern = r#""[^"]*""#]
	StrLit(Rc<str>, Span),
}

#[derive(PartialEq, Eq, Debug)]
pub enum TokenKind {
	Keyword,
	Ident,
	Brace,
	Punct,
	Operator,
	NumLit,
	StrLit,
}

impl Token {
	pub fn as_inner(&self) -> (&str, Span) {
		use Token::*;

		match self {
			Keyword(lexeme, span) => (&*lexeme, *span),
			Ident(lexeme, span) => (&*lexeme, *span),
			Brace(lexeme, span) => (&*lexeme, *span),
			Punct(lexeme, span) => (&*lexeme, *span),
			Operator(lexeme, span) => (&*lexeme, *span),
			NumLit(lexeme, span) => (&*lexeme, *span),
			StrLit(lexeme, span) => (&*lexeme, *span),
		}
	}

	// Constructors
	pub fn keyword<S>(lexeme: S, span: Span) -> Self
	where S: Into<Rc<str>> {
		Self::Keyword(lexeme.into(), span)
	}
	pub fn ident<S>(lexeme: S, span: Span) -> Self
	where S: Into<Rc<str>> {
		Self::Ident(lexeme.into(), span)
	}
	pub fn brace<S>(lexeme: S, span: Span) -> Self
	where S: Into<Rc<str>> {
		Self::Brace(lexeme.into(), span)
	}
	pub fn punct<S>(lexeme: S, span: Span) -> Self
	where S: Into<Rc<str>> {
		Self::Punct(lexeme.into(), span)
	}
	pub fn operator<S>(lexeme: S, span: Span) -> Self
	where S: Into<Rc<str>> {
		Self::Operator(lexeme.into(), span)
	}
	pub fn num_lit<S>(lexeme: S, span: Span) -> Self
	where S: Into<Rc<str>> {
		Self::NumLit(lexeme.into(), span)
	}
	pub fn str_lit<S>(lexeme: S, span: Span) -> Self
	where S: Into<Rc<str>> {
		Self::StrLit(lexeme.into(), span)
	}

	// Matchers
	pub fn match_keyword(input: Rc<str>) -> Option<_Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new(
				r"^(and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while)\b"
			)
			.unwrap();
		}
		PATTERN
			.find(&*input)
			.map(|m| _Match::new(Rc::clone(&input), m))
	}
	pub fn match_ident(input: Rc<str>) -> Option<_Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new("^([a-zA-Z_][a-zA-Z0-9_]*)").unwrap();
		}
		PATTERN
			.find(&*input)
			.map(|m| _Match::new(Rc::clone(&input), m))
	}
	pub fn match_brace(input: Rc<str>) -> Option<_Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new(r"^([(){}])").unwrap();
		}
		PATTERN
			.find(&*input)
			.map(|m| _Match::new(Rc::clone(&input), m))
	}
	pub fn match_punct(input: Rc<str>) -> Option<_Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new("^([,.;])").unwrap();
		}
		PATTERN
			.find(&*input)
			.map(|m| _Match::new(Rc::clone(&input), m))
	}
	pub fn match_operator(input: Rc<str>) -> Option<_Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new(r"^([=!<>]=?|[-+*/])").unwrap();
		}
		PATTERN
			.find(&*input)
			.map(|m| _Match::new(Rc::clone(&input), m))
	}
	pub fn match_num_lit(input: Rc<str>) -> Option<_Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new("^([0-9]+)").unwrap();
		}
		PATTERN
			.find(&*input)
			.map(|m| _Match::new(Rc::clone(&input), m))
	}
	pub fn match_str_lit(input: Rc<str>) -> Option<_Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new(r#"^("[^"]*")"#).unwrap();
		}
		PATTERN
			.find(&*input)
			.map(|m| _Match::new(Rc::clone(&input), m))
	}
}

#[macro_export]
macro_rules! brace {
	($lexeme:literal) => {
		Token::brace($lexeme, ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::brace(stringify!($lexeme), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! ident {
	($lexeme:literal) => {
		Token::ident($lexeme, ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::ident(stringify!($lexeme), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! keyword {
	($lexeme:literal) => {
		Token::keyword($lexeme, ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::keyword(stringify!($lexeme), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! num_lit {
	($lexeme:literal) => {
		Token::num_lit($lexeme, ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::num_lit(stringify!($lexeme), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! str_lit {
	($lexeme:literal) => {
		Token::str_lit($lexeme, ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::str_lit(stringify!($lexeme), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! operator {
	($lexeme:literal) => {
		Token::operator($lexeme, ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::operator(stringify!($lexeme), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! punct {
	($lexeme:literal) => {
		Token::punct($lexeme, ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::punct(stringify!($lexeme), ::gramatika::Span::default())
	};
}
pub use {brace, ident, keyword, num_lit, operator, punct, str_lit};

impl Clone for Token {
	fn clone(&self) -> Self {
		use Token::*;

		match self {
			Keyword(lexeme, span) => Keyword(Rc::clone(lexeme), *span),
			Ident(lexeme, span) => Ident(Rc::clone(lexeme), *span),
			Brace(lexeme, span) => Brace(Rc::clone(lexeme), *span),
			Punct(lexeme, span) => Punct(Rc::clone(lexeme), *span),
			Operator(lexeme, span) => Operator(Rc::clone(lexeme), *span),
			NumLit(lexeme, span) => NumLit(Rc::clone(lexeme), *span),
			StrLit(lexeme, span) => StrLit(Rc::clone(lexeme), *span),
		}
	}
}

impl gramatika::Token for Token {
	type Kind = TokenKind;

	fn lexeme(&self) -> &str {
		self.as_inner().0
	}

	fn kind(&self) -> Self::Kind {
		use Token::*;
		match self {
			Keyword(_, _) => TokenKind::Keyword,
			Ident(_, _) => TokenKind::Ident,
			Brace(_, _) => TokenKind::Brace,
			Punct(_, _) => TokenKind::Punct,
			Operator(_, _) => TokenKind::Operator,
			NumLit(_, _) => TokenKind::NumLit,
			StrLit(_, _) => TokenKind::StrLit,
		}
	}
}

impl Spanned for Token {
	fn span(&self) -> Span {
		self.as_inner().1
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.lexeme())
	}
}

impl DebugLisp for Token {
	fn fmt(&self, f: &mut fmt::Formatter, _: usize) -> fmt::Result {
		write!(
			f,
			"`{}` ({:?} ({:?}))",
			self.lexeme(),
			self.kind(),
			self.span(),
		)
	}
}

impl fmt::Debug for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		DebugLisp::fmt(self, f, 0)
	}
}
