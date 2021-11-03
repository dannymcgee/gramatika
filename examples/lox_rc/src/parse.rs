use std::rc::Rc;

use gramatika::{
	Lexer as _, ParseStreamer, Result, Spanned, SpannedError, Token as _, TokenCtor,
};

use crate::{
	lexer::Lexer,
	tokens::{Token, TokenKind},
};

pub struct ParseStream {
	input: Rc<str>,
	lexer: Lexer,
	peek: Option<Token>,
	tokens: Vec<Token>,
}

impl ParseStream {
	pub fn new(lexer: Lexer) -> Self {
		Self {
			input: lexer.source(),
			lexer,
			peek: None,
			tokens: vec![],
		}
	}

	pub fn source(&self) -> &'static str {
		let src = &*self.input;
		unsafe { std::mem::transmute(src) }
	}

	pub fn into_inner(self) -> (Rc<str>, Vec<Token>) {
		(self.input, self.tokens)
	}

	fn upcast(token: Token, convert: TokenCtor<Token>) -> Token {
		let (lexeme, span) = token.as_inner();
		convert(unsafe { std::mem::transmute(lexeme) }, span)
	}
}

impl<'a> ParseStreamer<'a> for ParseStream {
	type Token = Token;

	fn is_empty(&mut self) -> bool {
		if self.peek.is_some() {
			false
		} else {
			self.peek = self.lexer.scan_token();
			self.peek.is_none()
		}
	}

	fn peek(&mut self) -> Option<&Token> {
		if self.peek.is_none() {
			self.peek = self.lexer.scan_token();
		}
		self.peek.as_ref()
	}

	fn prev(&mut self) -> Option<&Token> {
		if self.tokens.is_empty() {
			None
		} else {
			Some(&self.tokens[self.tokens.len() - 1])
		}
	}

	fn check_kind(&mut self, kind: TokenKind) -> bool {
		self.peek().map_or(false, |token| kind == token.kind())
	}

	fn check(&mut self, compare: Token) -> bool {
		self.peek().map_or(false, |token| {
			token.kind() == compare.kind() && token.lexeme() == compare.lexeme()
		})
	}

	fn consume(&mut self, compare: Token) -> Result<'a, Token> {
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

	fn consume_kind(&mut self, kind: TokenKind) -> Result<'a, Token> {
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
		kind: TokenKind,
		convert: TokenCtor<'a, Token>,
	) -> Result<'a, Token> {
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
		kind: TokenKind,
		convert: TokenCtor<'a, Token>,
	) -> Result<'a, Token> {
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
		token: Token,
		convert: TokenCtor<'a, Token>,
	) -> Result<'a, Token> {
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

impl Iterator for ParseStream {
	type Item = Token;

	fn next(&mut self) -> Option<Token> {
		let next = if self.peek.is_some() {
			self.peek.take()
		} else {
			self.lexer.scan_token()
		}?;
		self.tokens.push(next.clone());

		Some(next)
	}
}

impl From<Lexer> for ParseStream {
	fn from(lexer: Lexer) -> Self {
		Self::new(lexer)
	}
}

impl<S> From<S> for ParseStream
where S: Into<Rc<str>>
{
	fn from(input: S) -> Self {
		let input: Rc<str> = input.into();
		let lexer = Lexer::new(Rc::clone(&input));

		Self {
			input,
			lexer,
			peek: None,
			tokens: vec![],
		}
	}
}
