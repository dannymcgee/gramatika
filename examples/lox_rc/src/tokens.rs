#![allow(unused_macros, dead_code)]

use std::fmt;

use arcstr::Substr;
use gramatika::{lazy_static, DebugLisp, Match, Regex, Span, Spanned, Token as _};

#[derive(PartialEq)]
pub enum Token {
	// #[pattern = r"(and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while)\b"]
	Keyword(Substr, Span),

	// #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(Substr, Span),

	// #[pattern = r"[(){}]"]
	Brace(Substr, Span),

	// #[pattern = "[,.;]"]
	Punct(Substr, Span),

	// #[pattern = "[=!<>]=?"]
	// #[pattern = "[-+*/]"]
	Operator(Substr, Span),

	// #[pattern = "[0-9]+"]
	NumLit(Substr, Span),

	// #[pattern = r#""[^"]*""#]
	StrLit(Substr, Span),
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
	pub fn as_inner(&self) -> (Substr, Span) {
		use Token::*;

		match self {
			Keyword(lexeme, span) => (lexeme.clone(), *span),
			Ident(lexeme, span) => (lexeme.clone(), *span),
			Brace(lexeme, span) => (lexeme.clone(), *span),
			Punct(lexeme, span) => (lexeme.clone(), *span),
			Operator(lexeme, span) => (lexeme.clone(), *span),
			NumLit(lexeme, span) => (lexeme.clone(), *span),
			StrLit(lexeme, span) => (lexeme.clone(), *span),
		}
	}

	// Constructors
	pub fn keyword(lexeme: Substr, span: Span) -> Self {
		Self::Keyword(lexeme, span)
	}
	pub fn ident(lexeme: Substr, span: Span) -> Self {
		Self::Ident(lexeme, span)
	}
	pub fn brace(lexeme: Substr, span: Span) -> Self {
		Self::Brace(lexeme, span)
	}
	pub fn punct(lexeme: Substr, span: Span) -> Self {
		Self::Punct(lexeme, span)
	}
	pub fn operator(lexeme: Substr, span: Span) -> Self {
		Self::Operator(lexeme, span)
	}
	pub fn num_lit(lexeme: Substr, span: Span) -> Self {
		Self::NumLit(lexeme, span)
	}
	pub fn str_lit(lexeme: Substr, span: Span) -> Self {
		Self::StrLit(lexeme, span)
	}

	// Matchers
	pub fn match_keyword(input: &str) -> Option<Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new(
				r"^(and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while)\b"
			)
			.unwrap();
		}
		PATTERN.find(input)
	}
	pub fn match_ident(input: &str) -> Option<Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new("^([a-zA-Z_][a-zA-Z0-9_]*)").unwrap();
		}
		PATTERN.find(input)
	}
	pub fn match_brace(input: &str) -> Option<Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new(r"^([(){}])").unwrap();
		}
		PATTERN.find(input)
	}
	pub fn match_punct(input: &str) -> Option<Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new("^([,.;])").unwrap();
		}
		PATTERN.find(input)
	}
	pub fn match_operator(input: &str) -> Option<Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new(r"^([=!<>]=?|[-+*/])").unwrap();
		}
		PATTERN.find(input)
	}
	pub fn match_num_lit(input: &str) -> Option<Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new("^([0-9]+)").unwrap();
		}
		PATTERN.find(input)
	}
	pub fn match_str_lit(input: &str) -> Option<Match> {
		lazy_static! {
			static ref PATTERN: Regex = Regex::new(r#"^("[^"]*")"#).unwrap();
		}
		PATTERN.find(input)
	}
}

#[macro_export]
macro_rules! brace {
	($lexeme:literal) => {
		Token::brace($lexeme.into(), ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::brace(stringify!($lexeme).into(), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! ident {
	($lexeme:literal) => {
		Token::ident($lexeme.into(), ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::ident(stringify!($lexeme).into(), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! keyword {
	($lexeme:literal) => {
		Token::keyword($lexeme.into(), ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::keyword(stringify!($lexeme).into(), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! num_lit {
	($lexeme:literal) => {
		Token::num_lit($lexeme.into(), ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::num_lit(stringify!($lexeme).into(), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! str_lit {
	($lexeme:literal) => {
		Token::str_lit($lexeme.into(), ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::str_lit(stringify!($lexeme).into(), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! operator {
	($lexeme:literal) => {
		Token::operator($lexeme.into(), ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::operator(stringify!($lexeme).into(), ::gramatika::Span::default())
	};
}
#[macro_export]
macro_rules! punct {
	($lexeme:literal) => {
		Token::punct($lexeme.into(), ::gramatika::Span::default())
	};
	($lexeme:tt) => {
		Token::punct(stringify!($lexeme).into(), ::gramatika::Span::default())
	};
}
pub use {brace, ident, keyword, num_lit, operator, punct, str_lit};

impl Clone for Token {
	fn clone(&self) -> Self {
		use Token::*;

		match self {
			Keyword(lexeme, span) => Keyword(lexeme.clone(), *span),
			Ident(lexeme, span) => Ident(lexeme.clone(), *span),
			Brace(lexeme, span) => Brace(lexeme.clone(), *span),
			Punct(lexeme, span) => Punct(lexeme.clone(), *span),
			Operator(lexeme, span) => Operator(lexeme.clone(), *span),
			NumLit(lexeme, span) => NumLit(lexeme.clone(), *span),
			StrLit(lexeme, span) => StrLit(lexeme.clone(), *span),
		}
	}
}

impl gramatika::Token for Token {
	type Kind = TokenKind;

	fn lexeme(&self) -> Substr {
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
