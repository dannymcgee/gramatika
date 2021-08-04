//! This binary only exists to debug the output of proc macros by running `cargo expand`
//! against it.

#![allow(unused_imports, dead_code)]

#[macro_use]
extern crate parse_framework;

use parse_framework::Span;

#[derive(Debug, Token, Lexer)]
enum Token<'a> {
	#[pattern(r"^(let|var)")]
	Keyword(&'a str, Span),

	#[pattern(r"^[a-zA-Z_][a-zA-Z0-9_]*")]
	Ident(&'a str, Span),

	#[pattern(r"^[;:{}()\[\]]")]
	Punct(&'a str, Span),

	#[pattern(r"^[-+*/=]")]
	Operator(&'a str, Span),

	#[pattern(r"^[0-9]+")]
	Literal(&'a str, Span),
}

// #[derive(Debug, Parse)]
// #[parse_token(crate::Token<'a>)]
// struct VarDecl<'a> {
// 	#[token(keyword![let])]
// 	pub storage: Token<'a>,

// 	#[token_kind(Ident)]
// 	pub ident: Token<'a>,

// 	#[token(operator![=])]
// 	pub eq: Token<'a>,

// 	#[token_kind(Literal)]
// 	pub expr: Token<'a>,

// 	#[token(punct![;])]
// 	pub semicolon: Token<'a>,
// }

fn main() {}
