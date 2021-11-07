#![allow(unused_macros, dead_code)]

use std::fmt;

use arcstr::Substr;
use gramatika::{DebugLisp, Span, Spanned, Token as _};
use lazy_static::lazy_static;
use regex::{Match, Regex};

#[derive(PartialEq)]
pub enum Token {
	// and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while
	Keyword(Keyword, Span),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
	And,
	Class,
	Else,
	False,
	For,
	Fun,
	If,
	Nil,
	Or,
	Print,
	Return,
	Super,
	This,
	True,
	Var,
	While,
}

impl Keyword {
	pub const AND: Substr = arcstr::literal_substr!("and");
	pub const CLASS: Substr = arcstr::literal_substr!("class");
	pub const ELSE: Substr = arcstr::literal_substr!("else");
	pub const FALSE: Substr = arcstr::literal_substr!("false");
	pub const FOR: Substr = arcstr::literal_substr!("for");
	pub const FUN: Substr = arcstr::literal_substr!("fun");
	pub const IF: Substr = arcstr::literal_substr!("if");
	pub const NIL: Substr = arcstr::literal_substr!("nil");
	pub const OR: Substr = arcstr::literal_substr!("or");
	pub const PRINT: Substr = arcstr::literal_substr!("print");
	pub const RETURN: Substr = arcstr::literal_substr!("return");
	pub const SUPER: Substr = arcstr::literal_substr!("super");
	pub const THIS: Substr = arcstr::literal_substr!("this");
	pub const TRUE: Substr = arcstr::literal_substr!("true");
	pub const VAR: Substr = arcstr::literal_substr!("var");
	pub const WHILE: Substr = arcstr::literal_substr!("while");
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
		match self {
			Token::Keyword(const_kw, span) => match *const_kw {
				Keyword::And => (Keyword::AND, *span),
				Keyword::Class => (Keyword::CLASS, *span),
				Keyword::Else => (Keyword::ELSE, *span),
				Keyword::False => (Keyword::FALSE, *span),
				Keyword::For => (Keyword::FOR, *span),
				Keyword::Fun => (Keyword::FUN, *span),
				Keyword::If => (Keyword::IF, *span),
				Keyword::Nil => (Keyword::NIL, *span),
				Keyword::Or => (Keyword::OR, *span),
				Keyword::Print => (Keyword::PRINT, *span),
				Keyword::Return => (Keyword::RETURN, *span),
				Keyword::Super => (Keyword::SUPER, *span),
				Keyword::This => (Keyword::THIS, *span),
				Keyword::True => (Keyword::TRUE, *span),
				Keyword::Var => (Keyword::VAR, *span),
				Keyword::While => (Keyword::WHILE, *span),
			},
			Token::Ident(lexeme, span) => (lexeme.clone(), *span),
			Token::Brace(lexeme, span) => (lexeme.clone(), *span),
			Token::Punct(lexeme, span) => (lexeme.clone(), *span),
			Token::Operator(lexeme, span) => (lexeme.clone(), *span),
			Token::NumLit(lexeme, span) => (lexeme.clone(), *span),
			Token::StrLit(lexeme, span) => (lexeme.clone(), *span),
		}
	}

	// Constructors
	pub fn keyword(lexeme: Substr, span: Span) -> Self {
		let const_kw = match lexeme {
			x if x == Keyword::AND => Keyword::And,
			x if x == Keyword::CLASS => Keyword::Class,
			x if x == Keyword::ELSE => Keyword::Else,
			x if x == Keyword::FALSE => Keyword::False,
			x if x == Keyword::FOR => Keyword::For,
			x if x == Keyword::FUN => Keyword::Fun,
			x if x == Keyword::IF => Keyword::If,
			x if x == Keyword::NIL => Keyword::Nil,
			x if x == Keyword::OR => Keyword::Or,
			x if x == Keyword::PRINT => Keyword::Print,
			x if x == Keyword::RETURN => Keyword::Return,
			x if x == Keyword::SUPER => Keyword::Super,
			x if x == Keyword::THIS => Keyword::This,
			x if x == Keyword::TRUE => Keyword::True,
			x if x == Keyword::VAR => Keyword::Var,
			x if x == Keyword::WHILE => Keyword::While,
			other => panic!("Invalid keyword: `{}`", other),
		};

		Self::Keyword(const_kw, span)
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
	(and) => {
		Token::Keyword(Keyword::And, ::gramatika::Span::default())
	};
	(class) => {
		Token::Keyword(Keyword::Class, ::gramatika::Span::default())
	};
	(else) => {
		Token::Keyword(Keyword::Else, ::gramatika::Span::default())
	};
	(false) => {
		Token::Keyword(Keyword::False, ::gramatika::Span::default())
	};
	(for) => {
		Token::Keyword(Keyword::For, ::gramatika::Span::default())
	};
	(fun) => {
		Token::Keyword(Keyword::Fun, ::gramatika::Span::default())
	};
	(if) => {
		Token::Keyword(Keyword::If, ::gramatika::Span::default())
	};
	(nil) => {
		Token::Keyword(Keyword::Nil, ::gramatika::Span::default())
	};
	(or) => {
		Token::Keyword(Keyword::Or, ::gramatika::Span::default())
	};
	(print) => {
		Token::Keyword(Keyword::Print, ::gramatika::Span::default())
	};
	(return) => {
		Token::Keyword(Keyword::Return, ::gramatika::Span::default())
	};
	(super) => {
		Token::Keyword(Keyword::Super, ::gramatika::Span::default())
	};
	(this) => {
		Token::Keyword(Keyword::This, ::gramatika::Span::default())
	};
	(true) => {
		Token::Keyword(Keyword::True, ::gramatika::Span::default())
	};
	(var) => {
		Token::Keyword(Keyword::Var, ::gramatika::Span::default())
	};
	(while) => {
		Token::Keyword(Keyword::While, ::gramatika::Span::default())
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
			Keyword(const_kw, span) => Keyword(*const_kw, *span),
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

	fn as_matchable(&self) -> (Self::Kind, &str, Span) {
		match self {
			Token::Keyword(const_kw, span) => {
				let kind = TokenKind::Keyword;

				match const_kw {
					Keyword::And => (kind, "and", *span),
					Keyword::Class => (kind, "class", *span),
					Keyword::Else => (kind, "else", *span),
					Keyword::False => (kind, "false", *span),
					Keyword::For => (kind, "for", *span),
					Keyword::Fun => (kind, "fun", *span),
					Keyword::If => (kind, "if", *span),
					Keyword::Nil => (kind, "nil", *span),
					Keyword::Or => (kind, "or", *span),
					Keyword::Print => (kind, "print", *span),
					Keyword::Return => (kind, "return", *span),
					Keyword::Super => (kind, "super", *span),
					Keyword::This => (kind, "this", *span),
					Keyword::True => (kind, "true", *span),
					Keyword::Var => (kind, "var", *span),
					Keyword::While => (kind, "while", *span),
				}
			}
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
