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
	LineComment(Substr, Span),
	// /\*.*?\*/
	BlockComment(Substr, Span),
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
	LineComment,
	BlockComment,
	Keyword,
	Ident,
	Brace,
	Punct,
	Operator,
	NumLit,
	StrLit,
}

macro_rules! count {
	() => (0_usize);
	($current:tt $($remaining:tt)*) => (1_usize + count!( $($remaining)* ));
}

macro_rules! hash_set {
	($($elem:path),+ $(,)?) => {{
		let mut set = ::std::collections::HashSet::with_capacity(count!( $($elem)* ));
		$( set.insert($elem); )+
		set
	}};
	() => {
		::std::collections::HashSet::default()
	};
}

impl TokenKind {
	pub(crate) fn discards() -> &'static ::std::collections::HashSet<TokenKind> {
		use ::std::collections::HashSet;

		static DISCARDS: OnceCell<HashSet<TokenKind>> = OnceCell::new();

		DISCARDS
			.get_or_init(|| hash_set![TokenKind::LineComment, TokenKind::BlockComment])
	}
	pub(crate) fn multilines() -> &'static ::std::collections::HashSet<TokenKind> {
		use ::std::collections::HashSet;

		static MULTILINES: OnceCell<HashSet<TokenKind>> = OnceCell::new();

		MULTILINES.get_or_init(|| hash_set![TokenKind::BlockComment])
	}
}

#[allow(clippy::type_complexity)]
impl Token {
	pub fn as_inner(&self) -> (Substr, Span) {
		match self {
			Token::LineComment(lexeme, span) => (lexeme.clone(), *span),
			Token::BlockComment(lexeme, span) => (lexeme.clone(), *span),
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
	pub fn line_comment(lexeme: Substr, span: Span) -> Self {
		Self::LineComment(lexeme, span)
	}
	pub fn block_comment(lexeme: Substr, span: Span) -> Self {
		Self::BlockComment(lexeme, span)
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

	#[inline]
	fn init_regex(
		pattern: &str,
		dotall: bool,
	) -> impl FnOnce() -> RwLock<Regex<SparseDFA<Vec<u8>, u32>>> + '_ {
		move || {
			let re = RegexBuilder::new()
				.anchored(true)
				.dot_matches_new_line(dotall)
				.build_sparse(pattern)
				.unwrap();

			let fwd = re.forward().to_u32().unwrap();
			let rev = re.reverse().to_u32().unwrap();

			RwLock::new(Regex::from_dfas(fwd, rev))
		}
	}

	// Pattern getters
	fn line_comment_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(Self::init_regex("//.*", false))
	}

	fn block_comment_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(Self::init_regex(r"/\*.*?\*/", true))
	}

	fn keyword_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(Self::init_regex(
			"and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while",
			false,
		))
	}

	fn ident_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(Self::init_regex("[a-zA-Z_][a-zA-Z0-9_]*", false))
	}

	fn brace_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(Self::init_regex(r"[(){}]", false))
	}

	fn punct_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(Self::init_regex(r"[,.;]", false))
	}

	fn operator_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(Self::init_regex(r"([=!<>]=?|[-+*/])", false))
	}

	fn num_lit_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(Self::init_regex(r"[0-9]+", false))
	}

	fn str_lit_pattern() -> &'static RwLock<Regex<SparseDFA<Vec<u8>, u32>>> {
		static PATTERN: OnceCell<RwLock<Regex<SparseDFA<Vec<u8>, u32>>>> =
			OnceCell::new();

		PATTERN.get_or_init(Self::init_regex("\"[^\"]*\"", false))
	}

	// Matchers
	pub fn match_line_comment(input: &str) -> Option<(usize, usize, TokenKind)> {
		Self::line_comment_pattern()
			.read()
			.unwrap()
			.find(input.as_bytes())
			.map(|(start, end)| (start, end, TokenKind::LineComment))
	}
	pub fn match_block_comment(input: &str) -> Option<(usize, usize, TokenKind)> {
		Self::block_comment_pattern()
			.read()
			.unwrap()
			.find(input.as_bytes())
			.map(|(start, end)| (start, end, TokenKind::BlockComment))
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

#[allow(unused_macros)]
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
#[allow(unused)]
pub(crate) use brace;

#[allow(unused_macros)]
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
#[allow(unused)]
pub(crate) use ident;

#[allow(unused_macros)]
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
#[allow(unused)]
pub(crate) use keyword;

#[allow(unused_macros)]
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
#[allow(unused)]
pub(crate) use num_lit;

#[allow(unused_macros)]
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
#[allow(unused)]
pub(crate) use str_lit;

#[allow(unused_macros)]
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
#[allow(unused)]
pub(crate) use operator;

#[allow(unused_macros)]
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
#[allow(unused)]
pub(crate) use punct;

impl Clone for Token {
	fn clone(&self) -> Self {
		use Token::*;

		match self {
			LineComment(lexeme, span) => LineComment(lexeme.clone(), *span),
			BlockComment(lexeme, span) => BlockComment(lexeme.clone(), *span),
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
			LineComment(_, _) => TokenKind::LineComment,
			BlockComment(_, _) => TokenKind::BlockComment,
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
			Token::LineComment(lex, span) => {
				(TokenKind::LineComment, lex.as_str(), *span)
			}
			Token::BlockComment(lex, span) => {
				(TokenKind::BlockComment, lex.as_str(), *span)
			}
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
