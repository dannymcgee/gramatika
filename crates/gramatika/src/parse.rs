use core::fmt;

use arcstr::{ArcStr, Substr};

use crate::{Lexer, Result, Span, Spanned, SpannedError, Token};

pub type TokenCtor<T> = fn(Substr, Span) -> T;

pub trait Parse
where Self: Sized
{
	type Stream: ParseStreamer;

	fn parse(input: &mut Self::Stream) -> Result<Self>;
}

pub trait ParseStreamer {
	type Token: Token + Spanned;

	fn parse<P>(&mut self) -> Result<P>
	where P: Parse<Stream = Self> {
		P::parse(self)
	}

	fn is_empty(&mut self) -> bool;
	fn peek(&mut self) -> Option<&Self::Token>;
	fn prev(&mut self) -> Option<&Self::Token>;
	fn check_kind(&mut self, kind: <Self::Token as Token>::Kind) -> bool;
	fn check(&mut self, compare: Self::Token) -> bool;
	fn consume(&mut self, compare: Self::Token) -> Result<Self::Token>;
	fn consume_kind(&mut self, kind: <Self::Token as Token>::Kind)
		-> Result<Self::Token>;

	fn consume_as(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token>;

	fn upgrade_last(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token>;

	fn upgrade(
		&mut self,
		token: Self::Token,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token>;

	fn discard(&mut self);
}

pub struct ParseStream<T, L>
where
	T: Token + Spanned,
	L: Lexer<Output = T>,
{
	input: ArcStr,
	lexer: L,
	peek: Option<T>,
	tokens: Vec<T>,
}

impl<T, L> ParseStream<T, L>
where
	T: Token + Spanned,
	L: Lexer<Output = T>,
{
	pub fn new(lexer: L) -> Self {
		Self {
			input: lexer.source(),
			lexer,
			peek: None,
			tokens: vec![],
		}
	}

	pub fn source(&self) -> ArcStr {
		ArcStr::clone(&self.input)
	}

	pub fn into_inner(self) -> (ArcStr, Vec<T>) {
		(self.input, self.tokens)
	}

	fn upcast(token: T, convert: TokenCtor<T>) -> T {
		let lexeme = token.lexeme();
		let span = token.span();

		convert(lexeme, span)
	}
}

// --- ParseStreamer impl ----------------------------------------------------------------

impl<T, L> ParseStreamer for ParseStream<T, L>
where
	T: Token + Spanned + fmt::Debug,
	L: Lexer<Output = T>,
{
	type Token = T;

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

	fn prev(&mut self) -> Option<&Self::Token> {
		if self.tokens.is_empty() {
			None
		} else {
			Some(&self.tokens[self.tokens.len() - 1])
		}
	}

	fn check_kind(&mut self, kind: <Self::Token as Token>::Kind) -> bool {
		self.peek().map_or(false, |token| kind == token.kind())
	}

	fn check(&mut self, compare: Self::Token) -> bool {
		self.peek().map_or(false, |token| {
			token.kind() == compare.kind() && token.lexeme() == compare.lexeme()
		})
	}

	fn consume(&mut self, compare: Self::Token) -> Result<Self::Token> {
		self.next()
			.map(|token| {
				if token.kind() == compare.kind() && token.lexeme() == compare.lexeme() {
					Ok(token)
				} else {
					Err(SpannedError {
						message: format!("Expected `{}`", compare.lexeme()),
						source: self.source(),
						span: Some(token.span()),
					})
				}
			})
			.unwrap_or_else(|| {
				Err(SpannedError {
					message: "Unexpected end of input".into(),
					source: self.source(),
					span: None,
				})
			})
	}

	fn consume_kind(
		&mut self,
		kind: <Self::Token as Token>::Kind,
	) -> Result<Self::Token> {
		self.next()
			.map(|token| {
				if token.kind() == kind {
					Ok(token)
				} else {
					Err(SpannedError {
						message: format!("Expected {:?}, found {:?}", kind, token.kind()),
						source: self.source(),
						span: Some(token.span()),
					})
				}
			})
			.unwrap_or_else(|| {
				Err(SpannedError {
					message: "Unexpected end of input".into(),
					source: self.source(),
					span: None,
				})
			})
	}

	fn consume_as(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token> {
		self.next()
			.and_then(|_| {
				let token = self.tokens.pop()?;
				if token.kind() == kind {
					let converted = Self::upcast(token, convert);
					self.tokens.push(converted.clone());

					Some(Ok(converted))
				} else {
					Some(Err(SpannedError {
						message: format!("Expected {:?}", kind),
						source: self.source(),
						span: Some(token.span()),
					}))
				}
			})
			.unwrap_or_else(|| {
				Err(SpannedError {
					message: "Unexpected end of input".into(),
					source: self.source(),
					span: None,
				})
			})
	}

	fn upgrade_last(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token> {
		self.tokens
			.pop()
			.map(|token| {
				if token.kind() == kind {
					let converted = Self::upcast(token, convert);
					self.tokens.push(converted.clone());

					Ok(converted)
				} else {
					Err(SpannedError {
						message: format!("Expected {:?}", kind),
						source: self.source(),
						span: Some(token.span()),
					})
				}
			})
			.unwrap_or_else(|| {
				Err(SpannedError {
					message: "Unexpected end of input".into(),
					source: self.source(),
					span: None,
				})
			})
	}

	fn upgrade(
		&mut self,
		token: Self::Token,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token> {
		let found = self
			.tokens
			.iter_mut()
			.find(|tok| tok.span() == token.span());
		if let Some(tok) = found {
			*tok = Self::upcast(token, convert);
			Ok(tok.clone())
		} else {
			panic!("Unable to find token in stream: {:?}", token);
		}
	}

	fn discard(&mut self) {
		let _ = self.next().unwrap();
	}
}

// --- Iterator --------------------------------------------------------------------------

impl<T, L> Iterator for ParseStream<T, L>
where
	T: Token + Spanned,
	L: Lexer<Output = T>,
{
	type Item = T;

	fn next(&mut self) -> Option<Self::Item> {
		let next = if self.peek.is_some() {
			self.peek.take()
		} else {
			self.lexer.scan_token()
		}?;
		self.tokens.push(next.clone());

		Some(next)
	}
}

// --- Conversions -----------------------------------------------------------------------

impl<S, T, L> From<S> for ParseStream<T, L>
where
	S: Into<ArcStr>,
	T: Token + Spanned,
	L: Lexer<Output = T>,
{
	fn from(input: S) -> Self {
		let input = input.into();
		let lexer = L::new(ArcStr::clone(&input));

		Self {
			input,
			lexer,
			peek: None,
			tokens: vec![],
		}
	}
}
