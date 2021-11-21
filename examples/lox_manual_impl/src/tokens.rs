#![allow(unused_macros, dead_code)]

use std::{fmt, sync::RwLock};

use arcstr::Substr;
use gramatika::{
	once_cell::sync::OnceCell,
	regex_automata::{Regex, RegexBuilder, SparseDFA},
	DebugLisp, Span, Spanned, Token as _,
};

#[derive(PartialEq)]
pub enum Token {
	// //.*
	Comment(Substr, Span),
	// and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while
	Keyword(Substr, Span),
	// [a-zA-Z_][a-zA-Z0-9_]*
	Ident(Substr, Span),
	// [(){}]
	Brace(Substr, Span),
	// [,.;]
	Punct(Substr, Span),
	// [=!<>]=?
	// [-+*/]
	Operator(Substr, Span),
	// [0-9]+
	NumLit(Substr, Span),
	// "[^"]*"
	StrLit(Substr, Span),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
	Comment,
	Keyword,
	Ident,
	Brace,
	Punct,
	Operator,
	NumLit,
	StrLit,
}

macro_rules! hash_set {
	($($elem:path),+ $(,)?) => {{
		let mut set = ::std::collections::HashSet::with_capacity(1);
		$( set.insert($elem); )+
		set
	}};
	() => {
		::std::collections::HashSet::with_capacity(1)
	};
}

impl TokenKind {
	pub(crate) fn discards() -> &'static ::std::collections::HashSet<TokenKind> {
		use ::std::collections::HashSet;

		static DISCARDS: OnceCell<HashSet<TokenKind>> = OnceCell::new();

		DISCARDS.get_or_init(|| hash_set![TokenKind::Comment])
	}
}

#[allow(clippy::type_complexity)]
impl Token {
	pub fn as_inner(&self) -> (Substr, Span) {
		match self {
			Token::Comment(lexeme, span) => (lexeme.clone(), *span),
			Token::Keyword(lexeme, span) => (lexeme.clone(), *span),
			Token::Ident(lexeme, span) => (lexeme.clone(), *span),
			Token::Brace(lexeme, span) => (lexeme.clone(), *span),
			Token::Punct(lexeme, span) => (lexeme.clone(), *span),
			Token::Operator(lexeme, span) => (lexeme.clone(), *span),
			Token::NumLit(lexeme, span) => (lexeme.clone(), *span),
			Token::StrLit(lexeme, span) => (lexeme.clone(), *span),
		}
	}

	// Constructors
	pub fn comment(lexeme: Substr, span: Span) -> Self {
		Self::Comment(lexeme, span)
	}
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

	// Pattern getters
	fn comment_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(|| {
			let re = RegexBuilder::new()
				.anchored(true)
				.build_sparse("//.*")
				.unwrap();

			let fwd = re.forward().to_u32().unwrap();
			let rev = re.reverse().to_u32().unwrap();

			RwLock::new(Regex::<SparseDFA<Vec<u8>, u32>>::from_dfas(fwd, rev))
		})
	}
	fn keyword_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(|| {
			let re = RegexBuilder::new()
				.anchored(true)
				.build_sparse("and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while")
				.unwrap();

			let fwd = re.forward().to_u32().unwrap();
			let rev = re.reverse().to_u32().unwrap();

			RwLock::new(Regex::<SparseDFA<Vec<u8>, u32>>::from_dfas(fwd, rev))
		})
	}
	fn ident_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(|| {
			let re = RegexBuilder::new()
				.anchored(true)
				.build_sparse("[a-zA-Z_][a-zA-Z0-9_]*")
				.unwrap();

			let fwd = re.forward().to_u32().unwrap();
			let rev = re.reverse().to_u32().unwrap();

			RwLock::new(Regex::<SparseDFA<Vec<u8>, u32>>::from_dfas(fwd, rev))
		})
	}
	fn brace_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(|| {
			let re = RegexBuilder::new()
				.anchored(true)
				.build_sparse(r"[(){}]")
				.unwrap();

			let fwd = re.forward().to_u32().unwrap();
			let rev = re.reverse().to_u32().unwrap();

			RwLock::new(Regex::<SparseDFA<Vec<u8>, u32>>::from_dfas(fwd, rev))
		})
	}
	fn punct_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(|| {
			let re = RegexBuilder::new()
				.anchored(true)
				.build_sparse(r"[,.;]")
				.unwrap();

			let fwd = re.forward().to_u32().unwrap();
			let rev = re.reverse().to_u32().unwrap();

			RwLock::new(Regex::<SparseDFA<Vec<u8>, u32>>::from_dfas(fwd, rev))
		})
	}
	fn operator_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(|| {
			let re = RegexBuilder::new()
				.anchored(true)
				.build_sparse(r"([=!<>]=?|[-+*/])")
				.unwrap();

			let fwd = re.forward().to_u32().unwrap();
			let rev = re.reverse().to_u32().unwrap();

			RwLock::new(Regex::<SparseDFA<Vec<u8>, u32>>::from_dfas(fwd, rev))
		})
	}
	fn num_lit_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(|| {
			let re = RegexBuilder::new()
				.anchored(true)
				.build_sparse(r"[0-9]+")
				.unwrap();

			let fwd = re.forward().to_u32().unwrap();
			let rev = re.reverse().to_u32().unwrap();

			RwLock::new(Regex::<SparseDFA<Vec<u8>, u32>>::from_dfas(fwd, rev))
		})
	}
	fn str_lit_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(|| {
			let re = RegexBuilder::new()
				.anchored(true)
				.build_sparse("\"[^\"]*\"")
				.unwrap();

			let fwd = re.forward().to_u32().unwrap();
			let rev = re.reverse().to_u32().unwrap();

			RwLock::new(Regex::<SparseDFA<Vec<u8>, u32>>::from_dfas(fwd, rev))
		})
	}

	// Matchers
	pub fn match_comment(input: &str) -> Option<(usize, usize, TokenKind)> {
		Self::comment_pattern()
			.read()
			.unwrap()
			.find(input.as_bytes())
			.map(|(start, end)| (start, end, TokenKind::Comment))
	}
	pub fn match_ident(input: &str) -> Option<(usize, usize, TokenKind)> {
		match Self::ident_pattern().read().unwrap().find(input.as_bytes()) {
			Some((start, end)) => match Self::keyword_pattern()
				.read()
				.unwrap()
				.find(input.as_bytes())
			{
				Some((s, e)) if s == start && e == end => {
					Some((start, end, TokenKind::Keyword))
				}
				_ => Some((start, end, TokenKind::Ident)),
			},
			None => None,
		}
	}
	pub fn match_brace(input: &str) -> Option<(usize, usize, TokenKind)> {
		Self::brace_pattern()
			.read()
			.unwrap()
			.find(input.as_bytes())
			.map(|(start, end)| (start, end, TokenKind::Brace))
	}
	pub fn match_punct(input: &str) -> Option<(usize, usize, TokenKind)> {
		Self::punct_pattern()
			.read()
			.unwrap()
			.find(input.as_bytes())
			.map(|(start, end)| (start, end, TokenKind::Punct))
	}
	pub fn match_operator(input: &str) -> Option<(usize, usize, TokenKind)> {
		Self::operator_pattern()
			.read()
			.unwrap()
			.find(input.as_bytes())
			.map(|(start, end)| (start, end, TokenKind::Operator))
	}
	pub fn match_num_lit(input: &str) -> Option<(usize, usize, TokenKind)> {
		Self::num_lit_pattern()
			.read()
			.unwrap()
			.find(input.as_bytes())
			.map(|(start, end)| (start, end, TokenKind::NumLit))
	}
	pub fn match_str_lit(input: &str) -> Option<(usize, usize, TokenKind)> {
		Self::str_lit_pattern()
			.read()
			.unwrap()
			.find(input.as_bytes())
			.map(|(start, end)| (start, end, TokenKind::StrLit))
	}
}

#[macro_export]
macro_rules! brace {
	($lexeme:literal) => {
		Token::brace(
			::gramatika::arcstr::literal_substr!($lexeme),
			::gramatika::Span::default(),
		)
	};
	($lexeme:tt) => {
		Token::brace(
			::gramatika::arcstr::literal_substr!(stringify!($lexeme)),
			::gramatika::Span::default(),
		)
	};
}
#[macro_export]
macro_rules! ident {
	($lexeme:literal) => {
		Token::ident(
			::gramatika::arcstr::literal_substr!($lexeme),
			::gramatika::Span::default(),
		)
	};
	($lexeme:tt) => {
		Token::ident(
			::gramatika::arcstr::literal_substr!(stringify!($lexeme)),
			::gramatika::Span::default(),
		)
	};
}
#[macro_export]
macro_rules! keyword {
	($lexeme:literal) => {
		Token::keyword(
			::gramatika::arcstr::literal_substr!($lexeme),
			::gramatika::Span::default(),
		)
	};
	($lexeme:tt) => {
		Token::keyword(
			::gramatika::arcstr::literal_substr!(stringify!($lexeme)),
			::gramatika::Span::default(),
		)
	};
}
#[macro_export]
macro_rules! num_lit {
	($lexeme:literal) => {
		Token::num_lit(
			::gramatika::arcstr::literal_substr!($lexeme),
			::gramatika::Span::default(),
		)
	};
	($lexeme:tt) => {
		Token::num_lit(
			::gramatika::arcstr::literal_substr!(stringify!($lexeme)),
			::gramatika::Span::default(),
		)
	};
}
#[macro_export]
macro_rules! str_lit {
	($lexeme:literal) => {
		Token::str_lit(
			::gramatika::arcstr::literal_substr!($lexeme),
			::gramatika::Span::default(),
		)
	};
	($lexeme:tt) => {
		Token::str_lit(
			::gramatika::arcstr::literal_substr!(stringify!($lexeme)),
			::gramatika::Span::default(),
		)
	};
}
#[macro_export]
macro_rules! operator {
	($lexeme:literal) => {
		Token::operator(
			::gramatika::arcstr::literal_substr!($lexeme),
			::gramatika::Span::default(),
		)
	};
	($lexeme:tt) => {
		Token::operator(
			::gramatika::arcstr::literal_substr!(stringify!($lexeme)),
			::gramatika::Span::default(),
		)
	};
}
#[macro_export]
macro_rules! punct {
	($lexeme:literal) => {
		Token::punct(
			::gramatika::arcstr::literal_substr!($lexeme),
			::gramatika::Span::default(),
		)
	};
	($lexeme:tt) => {
		Token::punct(
			::gramatika::arcstr::literal_substr!(stringify!($lexeme)),
			::gramatika::Span::default(),
		)
	};
}
pub use {brace, ident, keyword, num_lit, operator, punct, str_lit};

impl Clone for Token {
	fn clone(&self) -> Self {
		use Token::*;

		match self {
			Comment(lexeme, span) => Comment(lexeme.clone(), *span),
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
			Comment(_, _) => TokenKind::Comment,
			Keyword(_, _) => TokenKind::Keyword,
			Ident(_, _) => TokenKind::Ident,
			Brace(_, _) => TokenKind::Brace,
			Punct(_, _) => TokenKind::Punct,
			Operator(_, _) => TokenKind::Operator,
			NumLit(_, _) => TokenKind::NumLit,
			StrLit(_, _) => TokenKind::StrLit,
		}
	}

	fn as_matchable(&self) -> (Self::Kind, &str, Span) {
		match self {
			Token::Comment(lex, span) => (TokenKind::Comment, lex.as_str(), *span),
			Token::Keyword(lex, span) => (TokenKind::Keyword, lex.as_str(), *span),
			Token::Ident(lex, span) => (TokenKind::Ident, lex.as_str(), *span),
			Token::Brace(lex, span) => (TokenKind::Brace, lex.as_str(), *span),
			Token::Punct(lex, span) => (TokenKind::Punct, lex.as_str(), *span),
			Token::Operator(lex, span) => (TokenKind::Operator, lex.as_str(), *span),
			Token::NumLit(lex, span) => (TokenKind::NumLit, lex.as_str(), *span),
			Token::StrLit(lex, span) => (TokenKind::StrLit, lex.as_str(), *span),
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
