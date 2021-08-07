use std::fmt;

use crate::Span;

pub trait Lexer {
	type Input;
	type Output: Token;

	fn new(input: Self::Input) -> Self;
	fn source(&self) -> Self::Input;
	fn scan(&mut self) -> Vec<Self::Output>;
	fn scan_token(&mut self) -> Option<Self::Output>;
}

pub trait Token {
	type Kind: fmt::Debug + PartialEq;

	fn span(&self) -> Span;
	fn lexeme(&self) -> &str;
	fn kind(&self) -> Self::Kind;
}
