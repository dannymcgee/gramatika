use crate::{Lexer, Result, SpannedError, Token};

pub trait Parse<'a> {
	type Stream: ParseStreamer<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self>
	where
		Self: Sized;
}

pub trait ParseStreamer<'a> {
	type Token: crate::Token;

	fn parse<P: Parse<'a, Stream = Self>>(&mut self) -> Result<'a, P>;
	fn is_empty(&mut self) -> bool;
	fn peek(&mut self) -> Option<&Self::Token>;
	fn check_kind(&mut self, kind: <Self::Token as crate::Token>::Kind) -> bool;
	fn check(&mut self, compare: Self::Token) -> bool;
	fn consume(&mut self, compare: Self::Token) -> Result<'a, Self::Token>;
	fn consume_kind(
		&mut self,
		kind: <Self::Token as crate::Token>::Kind,
	) -> Result<'a, Self::Token>;
}

pub struct ParseStream<'a, T, L>
where
	T: Token,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	input: &'a str,
	lexer: L,
	peek: Option<T>,
}

impl<'a, T, L> ParseStream<'a, T, L>
where
	T: Token,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	pub fn new(lexer: L) -> Self {
		Self {
			input: lexer.source(),
			lexer,
			peek: None,
		}
	}

	pub fn source(&self) -> &'a str {
		self.input
	}
}

// --- ParseStreamer impl ----------------------------------------------------------------

impl<'a, T, L> ParseStreamer<'a> for ParseStream<'a, T, L>
where
	T: Token,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	type Token = T;

	fn parse<P: Parse<'a, Stream = Self>>(&mut self) -> Result<'a, P> {
		P::parse(self)
	}

	fn is_empty(&mut self) -> bool {
		if self.peek.is_some() {
			false
		} else {
			self.peek = self.lexer.scan_token();
			self.peek.is_none()
		}
	}

	fn peek(&mut self) -> Option<&Self::Token> {
		if self.peek.is_none() {
			self.peek = self.lexer.scan_token();
		}
		self.peek.as_ref()
	}

	fn check_kind(&mut self, kind: <Self::Token as Token>::Kind) -> bool {
		self.peek().map_or(false, |token| kind == token.kind())
	}

	fn check(&mut self, compare: Self::Token) -> bool {
		self.peek().map_or(false, |token| {
			token.kind() == compare.kind() && token.lexeme() == compare.lexeme()
		})
	}

	fn consume(&mut self, compare: Self::Token) -> Result<'a, Self::Token> {
		self.next()
			.map(|token| {
				if token.kind() == compare.kind() && token.lexeme() == compare.lexeme() {
					Ok(token)
				} else {
					Err(SpannedError {
						message: format!("Expected `{}`", compare.lexeme()),
						source: self.input,
						span: Some(token.span()),
					})
				}
			})
			.unwrap_or_else(|| {
				Err(SpannedError {
					message: "Unexpected end of input".into(),
					source: self.input,
					span: None,
				})
			})
	}

	fn consume_kind(
		&mut self,
		kind: <Self::Token as Token>::Kind,
	) -> Result<'a, Self::Token> {
		self.next()
			.map(|token| {
				if token.kind() == kind {
					Ok(token)
				} else {
					Err(SpannedError {
						message: format!("Expected {:?}", kind),
						source: self.input,
						span: Some(token.span()),
					})
				}
			})
			.unwrap_or_else(|| {
				Err(SpannedError {
					message: "Unexpected end of input".into(),
					source: self.input,
					span: None,
				})
			})
	}
}

// --- Iterator --------------------------------------------------------------------------

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
			self.lexer.scan_token()
		}
	}
}

// --- Conversions -----------------------------------------------------------------------

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

		Self {
			input,
			lexer,
			peek: None,
		}
	}
}
