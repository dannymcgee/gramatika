use std::fmt;

use crate::{Span, TokenStream};

pub trait Lexer {
	type Input;
	type Output: Token;

	fn scan(&mut self) -> TokenStream<Self::Output>;
	fn scan_token(&mut self) -> Option<Self::Output>;
}

pub trait Token {
	type Kind: fmt::Debug + PartialEq;

	fn span(&self) -> Span;
	fn lexeme(&self) -> &str;
	fn kind(&self) -> Self::Kind;
}
