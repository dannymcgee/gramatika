use core::fmt;
use std::mem;

use crate::{Lexer, Result, Span, Spanned, SpannedError, Token};

pub trait Parse<'a>
where Self: Sized
{
	type Stream: ParseStreamer<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self>;
}

pub trait ParseStreamer<'a> {
	type Token: Token + Spanned;

	fn parse<P: Parse<'a, Stream = Self>>(&mut self) -> Result<'a, P>;
	fn is_empty(&mut self) -> bool;
	fn peek(&mut self) -> Option<&Self::Token>;
	fn prev(&mut self) -> Option<&Self::Token>;
	fn check_kind(&mut self, kind: <Self::Token as Token>::Kind) -> bool;
	fn check(&mut self, compare: Self::Token) -> bool;
	fn consume(&mut self, compare: Self::Token) -> Result<'a, Self::Token>;
	fn consume_kind(
		&mut self,
		kind: <Self::Token as Token>::Kind,
	) -> Result<'a, Self::Token>;
	fn consume_as(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: fn(&'a str, Span) -> Self::Token,
	) -> Result<'a, Self::Token>;
	fn upgrade_last(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: fn(&'a str, Span) -> Self::Token,
	) -> Result<'a, Self::Token>;
	fn upgrade(
		&mut self,
		token: Self::Token,
		convert: fn(&'a str, Span) -> Self::Token,
	) -> Result<'a, Self::Token>;
	fn discard(&mut self);
}

pub struct ParseStream<'a, T, L>
where
	T: Token + Spanned,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	input: String,
	lexer: L,
	peek: Option<T>,
	tokens: Vec<T>,
}

impl<'a, T, L> ParseStream<'a, T, L>
where
	T: Token + Spanned + Copy,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	pub fn new(lexer: L) -> Self {
		Self {
			input: lexer.source().into(),
			lexer,
			peek: None,
			tokens: vec![],
		}
	}

	pub fn source(&self) -> &'a str {
		// This struct owns the String that the returned `&str` references, which is the
		// same String that's leant to our lexer as `&'a str`, so unless I'm very badly
		// mistaken, the lifetime of `&self.input` == `'a`
		unsafe { with_lifetime(&self.input) }
	}

	pub fn into_inner(self) -> (String, Vec<T>) {
		(self.input, self.tokens)
	}

	fn upcast(token: T, convert: fn(&'a str, Span) -> T) -> T {
		// T and L are linked in that L is constrained by `Lexer<Input = &'a str, Output = T>`
		// I suppose it would be possible for a user to implement Lexer and Token in such a
		// way that `Token::lexeme` returns a `&str` with a lifetime that's shorter than
		// the one bound to `Lexer::Input`, but that's not a use case I'm overly concerned
		// with at the moment.
		let lexeme = unsafe { with_lifetime(token.lexeme()) };
		let span = token.span();

		convert(lexeme, span)
	}
}

// --- ParseStreamer impl ----------------------------------------------------------------

impl<'a, T, L> ParseStreamer<'a> for ParseStream<'a, T, L>
where
	T: Token + Spanned + Copy + fmt::Debug,
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

	fn consume(&mut self, compare: Self::Token) -> Result<'a, Self::Token> {
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
	) -> Result<'a, Self::Token> {
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
		convert: fn(&'a str, Span) -> Self::Token,
	) -> Result<'a, Self::Token> {
		self.next()
			.and_then(|_| {
				let token = self.tokens.pop()?;
				if token.kind() == kind {
					let converted = Self::upcast(token, convert);
					self.tokens.push(converted);

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
		convert: fn(&'a str, Span) -> Self::Token,
	) -> Result<'a, Self::Token> {
		self.tokens
			.pop()
			.map(|token| {
				if token.kind() == kind {
					let converted = Self::upcast(token, convert);
					self.tokens.push(converted);

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
		convert: fn(&'a str, Span) -> Self::Token,
	) -> Result<'a, Self::Token> {
		let found = self
			.tokens
			.iter_mut()
			.find(|tok| tok.span() == token.span());
		if let Some(tok) = found {
			*tok = Self::upcast(token, convert);
			Ok(*tok)
		} else {
			panic!("Unable to find token in stream: {:?}", token);
		}
	}

	fn discard(&mut self) {
		let _ = self.next().unwrap();
	}
}

// --- Iterator --------------------------------------------------------------------------

impl<'a, T, L> Iterator for ParseStream<'a, T, L>
where
	T: Token + Spanned + Copy,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	type Item = T;

	fn next(&mut self) -> Option<Self::Item> {
		let next = if self.peek.is_some() {
			self.peek.take()
		} else {
			self.lexer.scan_token()
		}?;
		self.tokens.push(next);

		Some(next)
	}
}

// --- Conversions -----------------------------------------------------------------------

impl<'a, T, L> From<L> for ParseStream<'a, T, L>
where
	T: Token + Spanned + Copy,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	fn from(lexer: L) -> Self {
		Self::new(lexer)
	}
}

impl<'a, T, L> From<&'a str> for ParseStream<'a, T, L>
where
	T: Token + Spanned,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	fn from(input: &'a str) -> Self {
		let lexer = L::new(input);

		Self {
			input: input.into(),
			lexer,
			peek: None,
			tokens: vec![],
		}
	}
}

impl<'a, T, L> From<String> for ParseStream<'a, T, L>
where
	T: Token + Spanned,
	L: Lexer<Input = &'a str, Output = T> + Sized,
{
	fn from(input: String) -> Self {
		// The compiler complains that we're moving out of a shared reference (when giving
		// `input` to `Self` below), and that `input` doesn't live long enough because it's
		// dropped at the end of this function. It is very much not dropped, because we are
		// taking ownership of both it and the lexer.
		//
		// In reality, the borrow will live at least as long as the ParseStream we're
		// returning, and potentially longer via its `into_inner` method, which consumes it
		// and returns both the backing String and the tokens scanned from it.
		//
		// FIXME: It would technically be possible for the user to do something unexpected
		// like call `into_inner`, drop the returned String, and continue trying to use the
		// returned tokens, which would (I think?) eventually lead to those tokens pointing
		// to memory that's no longer valid. It might be worth adding some warnings to
		// indicate how this needs to be used in order to remain safe, or else figuring out
		// how to constrain the lifetime of the tokens by the lifetime of `input`.
		let lexer = L::new(unsafe { with_lifetime(input.as_str()) });

		Self {
			input,
			lexer,
			peek: None,
			tokens: vec![],
		}
	}
}

/// This is some seriously hacky bullshit, but we only actually use it to assert that the
/// lifetime of some &str is equal to an actual, known lifetime in a handful of places
/// where we know that to be true but the compiler is unable to statically verify it.
unsafe fn with_lifetime<'a, 'b>(string: &'b str) -> &'a str
where 'a: 'b {
	mem::transmute::<&'b str, &'a str>(string)
}
