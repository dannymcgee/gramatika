//! This crate contains a simple parser for [Lox](https://craftinginterpreters.com/) as an
//! end-to-end and usability test of the tooling in the `parse_framework` crate.

#![allow(unused_imports)]

#[macro_use]
extern crate parse_framework;

use parse_framework::{Lexer as _, Parse, Token as _};

mod decl;
mod expr;
mod stmt;
mod tokens;

use decl::*;
use expr::*;
use stmt::*;
use tokens::*;

pub struct ParseStream<'a> {
	inner: Lexer<'a>,
	peek: Option<Token<'a>>,
}

impl<'a> ParseStream<'a> {
	pub fn new(lexer: Lexer<'a>) -> Self {
		Self {
			inner: lexer,
			peek: None,
		}
	}
}

impl<'a> From<Lexer<'a>> for ParseStream<'a> {
	fn from(lexer: Lexer<'a>) -> Self {
		Self::new(lexer)
	}
}

impl<'a> From<&'a str> for ParseStream<'a> {
	fn from(input: &'a str) -> Self {
		let lexer = Lexer::new(input);

		Self::new(lexer)
	}
}

impl<'a> parse_framework::ParseStream for ParseStream<'a> {
	type Token = Token<'a>;

	fn parse<P: Parse<Stream = Self>>(&mut self) -> Result<P, String> {
		P::parse(self)
	}

	fn is_empty(&mut self) -> bool {
		if self.peek.is_some() {
			false
		} else {
			self.peek = self.inner.scan_token();
			self.peek.is_none()
		}
	}

	fn peek(&mut self) -> Option<&Self::Token> {
		if self.peek.is_none() {
			self.peek = self.inner.scan_token();
		}
		self.peek.as_ref()
	}

	fn check_kind(
		&mut self,
		kind: <Self::Token as parse_framework::Token>::Kind,
	) -> bool {
		self.peek()
			.map(|token| kind == token.kind())
			.unwrap_or(false)
	}

	fn check(&mut self, compare: Self::Token) -> bool {
		self.peek()
			.map(|token| {
				token.kind() == compare.kind() && token.lexeme() == compare.lexeme()
			})
			.unwrap_or(false)
	}

	fn consume(&mut self, compare: Self::Token) -> Result<Self::Token, String> {
		self.next()
			.map(|token| {
				if token.kind() == compare.kind() && token.lexeme() == compare.lexeme() {
					Ok(token)
				} else {
					let detail = if token.kind() != compare.kind() {
						format!("Kind {:?} != {:?}", token.kind(), compare.kind())
					} else {
						format!("Lexeme `{}` != `{}`", token.lexeme(), compare.lexeme())
					};

					Err(format!(
						"Expected {:?} `{}` but found {:?}\n{}",
						compare.kind(),
						compare.lexeme(),
						token,
						detail,
					))
				}
			})
			.unwrap_or_else(|| Err("Unexpected end of input".into()))
	}

	fn consume_kind(
		&mut self,
		kind: <Self::Token as parse_framework::Token>::Kind,
	) -> Result<Self::Token, String> {
		self.next()
			.map(|token| {
				if token.kind() == kind {
					Ok(token)
				} else {
					Err(format!("Expected {:?} but found {:?}", kind, token))
				}
			})
			.unwrap_or_else(|| Err("Unexpected end of input".into()))
	}
}

impl<'a> Iterator for ParseStream<'a> {
	type Item = Token<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.peek.is_some() {
			self.peek.take()
		} else {
			self.inner.scan_token()
		}
	}
}

#[cfg(test)]
mod tests;
