use std::fmt;

use crate::{Lexer, Token};

pub trait Parse {
	type Stream: ParseStreamer;

	fn parse(input: &mut Self::Stream) -> Result<Self, String>
	where Self: Sized;
}

pub trait ParseStreamer {
	type Token: crate::Token;

	fn parse<P: Parse<Stream = Self>>(&mut self) -> Result<P, String>;
	fn is_empty(&mut self) -> bool;
	fn peek(&mut self) -> Option<&Self::Token>;
	fn check_kind(&mut self, kind: <Self::Token as crate::Token>::Kind) -> bool;
	fn check(&mut self, compare: Self::Token) -> bool;
	fn consume(&mut self, compare: Self::Token) -> Result<Self::Token, String>;
	fn consume_kind(
		&mut self,
		kind: <Self::Token as crate::Token>::Kind,
	) -> Result<Self::Token, String>;
}

pub struct ParseStream<'a, T, L>
where
	T: Token,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	inner: L,
	peek: Option<T>,
}

impl<'a, T, L> ParseStream<'a, T, L>
where
	T: Token,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	pub fn new(lexer: L) -> Self {
		Self {
			inner: lexer,
			peek: None,
		}
	}
}

impl<'a, T, L> ParseStreamer for ParseStream<'a, T, L>
where
	T: Token + fmt::Debug,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	type Token = T;

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

	fn check_kind(&mut self, kind: <Self::Token as Token>::Kind) -> bool {
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
		kind: <Self::Token as Token>::Kind,
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

impl<'a, T, L> Iterator for ParseStream<'a, T, L>
where
	T: Token,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	type Item = T;

	fn next(&mut self) -> Option<Self::Item> {
		if self.peek.is_some() {
			self.peek.take()
		} else {
			self.inner.scan_token()
		}
	}
}

impl<'a, T, L> From<L> for ParseStream<'a, T, L>
where
	T: Token,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	fn from(lexer: L) -> Self {
		Self::new(lexer)
	}
}

impl<'a, T, L> From<&'a str> for ParseStream<'a, T, L>
where
	T: Token,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	fn from(input: &'a str) -> Self {
		let lexer = L::new(input);

		Self::new(lexer)
	}
}
