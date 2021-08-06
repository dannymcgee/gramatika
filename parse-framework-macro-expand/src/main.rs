//! This binary only exists to debug the output of proc macros by running `cargo expand`
//! against it.

#![allow(unused_imports, dead_code)]

#[macro_use]
extern crate parse_framework;

use parse_framework::Span;

#[derive(Debug, Token, DebugLispToken)]
pub enum Token<'a> {
	#[pattern(r"^(let|var|if|for|while|return)")]
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

#[derive(DebugLisp)]
pub enum Expr<'a> {
	Binary(BinaryExpr<'a>),
	Unary(UnaryExpr<'a>),
	Primary(PrimaryExpr<'a>),
}

#[derive(DebugLisp)]
pub struct BinaryExpr<'a> {
	pub lhs: Box<Expr<'a>>,
	pub op: Token<'a>,
	pub rhs: Box<Expr<'a>>,
}

#[derive(DebugLisp)]
pub struct UnaryExpr<'a> {
	pub op: Token<'a>,
	pub rhs: Box<Expr<'a>>,
}

#[derive(DebugLisp)]
pub struct PrimaryExpr<'a> {
	pub token: Token<'a>,
}

fn main() {}
