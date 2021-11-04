use std::fmt;

use arcstr::{ArcStr, Substr};

pub trait Lexer {
	type Output: Token;

	fn new(input: ArcStr) -> Self;
	fn source(&self) -> ArcStr;
	fn scan(&mut self) -> Vec<Self::Output>;
	fn scan_token(&mut self) -> Option<Self::Output>;
}

pub trait Token
where Self: Clone
{
	type Kind: fmt::Debug + PartialEq;

	fn lexeme(&self) -> Substr;
	fn kind(&self) -> Self::Kind;
}
