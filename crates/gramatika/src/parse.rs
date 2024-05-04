use core::fmt;

use arcstr::{ArcStr, Substr};

use crate::{Lexer, Position, Result, Span, Spanned, SpannedError, Token};

pub type TokenCtor<T> = fn(Substr, Span) -> T;

pub trait Parse
where Self: Sized
{
	type Stream: ParseStreamer;

	fn parse(input: &mut Self::Stream) -> Result<Self>;
}

pub trait ParseStreamer {
	type Token: Token + Spanned;

	#[allow(unused_variables)]
	fn with_runtime_matcher<F>(self, matcher: F) -> Self
	where
		Self: Sized,
		F: Fn(&str) -> Option<(usize, <Self::Token as Token>::Kind)> + 'static,
	{
		self
	}

	/// Provides a more convenient API for parsing other implementers of the [`Parse`]
	/// trait.
	///
	/// ```
	/// impl Parse for MyParentNode {
	///     type Stream = MyStream;
	///
	///     fn parse(input: &mut Self::Stream) -> Result<Self> {
	///         let foo_child = input.parse()?;
	///         let bar_child = input.parse()?;
	///         let baz_child = input.parse()?;
	///
	///         Ok(Self {
	///             foo_child,
	///             bar_child,
	///             baz_child,
	///         })
	///     }
	/// }
	/// ```
	fn parse<P>(&mut self) -> Result<P>
	where P: Parse<Stream = Self> {
		P::parse(self)
	}

	/// Returns `true` when there are no more tokens in the stream.
	fn is_empty(&mut self) -> bool;

	/// Returns a [`Some`] reference to the next [`Token`] in the stream without advancing
	/// the iterator, or [`None`] if the stream is empty.
	fn peek(&mut self) -> Option<&Self::Token>;

	/// Returns a [`Some`] reference to the last token consumed by the iterator. Returns
	/// [`None`] if the source string contains no tokens, or if no tokens have been
	/// consumed yet.
	///
	/// Underlying data access is `O(1)` in the [crate-provided implementation].
	///
	/// [crate-provided implementation]: ParseStream
	fn prev(&mut self) -> Option<&Self::Token>;

	/// Indicates whether the next [`Token`] in the stream matches the given [`Kind`],
	/// without advancing the iterator.
	///
	/// [`Kind`]: Token::Kind
	fn check_kind(&mut self, kind: <Self::Token as Token>::Kind) -> bool;

	/// Indicates whether the next [`Token`] in the stream matches the parameter by
	/// comparing their [`lexeme`]s. Does not advance the iterator.
	///
	/// [`lexeme`]: Token::lexeme
	fn check(&mut self, compare: Self::Token) -> bool;

	/// Advances the iterator, returning [`Ok`] with the next [`Token`] if it matches the
	/// parameter by comparing their [`lexeme`]s. Otherwise returns a contextual
	/// [`Err`]`(`[`SpannedError`]`)`.
	///
	/// [`lexeme`]: Token::lexeme
	fn consume(&mut self, compare: Self::Token) -> Result<Self::Token>;

	/// Advances the iterator, returning [`Ok`] with the next [`Token`] if it matches the
	/// given [`Kind`]. Otherwise returns a contextual [`Err`]`(`[`SpannedError`]`)`.
	///
	/// [`Kind`]: Token::Kind
	fn consume_kind(&mut self, kind: <Self::Token as Token>::Kind)
		-> Result<Self::Token>;

	/// ### TODO:
	/// * Docs
	/// * Consider moving to a different trait or providing a default impl to reduce the
	///   burden of manually implementing this trait.
	fn consume_as(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token>;

	/// ### TODO:
	/// * Docs
	/// * Consider moving to a different trait or providing a default impl to reduce the
	///   burden of manually implementing this trait.
	fn upgrade_last(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token>;

	/// ### TODO:
	/// * Docs
	/// * Consider moving to a different trait or providing a default impl to reduce the
	///   burden of manually implementing this trait.
	fn upgrade(
		&mut self,
		token: Self::Token,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token>;

	/// Advances the iterator, ignoring the next [`Token`].
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

	/// Advances the iterator by splitting the next token returned by the lexer
	/// in two at the index indicated by `split_at`.
	///
	/// This function returns the result of calling `ctor.0` with the scanned
	/// token's substring from `(..split_at)`, and the `peek` buffer is
	/// populated with the result of calling `ctor.1` with `(split_at..)`.
	///
	/// # Examples
	///
	/// ```
	/// # #[macro_use] extern crate gramatika_macro;
	/// # fn main () {
	/// use gramatika::{span, Substr, Span, ParseStream, ParseStreamer};
	///
	/// #[derive(Token, Lexer, Debug, PartialEq, Eq)]
	/// enum Token {
	///     #[pattern = "ab"]
	///     Ab(Substr, Span),
	///     #[pattern = "a"]
	///     A(Substr, Span),
	///     #[pattern = "b"]
	///     B(Substr, Span),
	/// }
	///
	/// let input = "ab";
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	///
	/// assert_eq!(parser.peek(), Some(&Token::Ab("ab".into(), span![1:1..1:3])));
	///
	/// let a = parser.split_next(1, (Token::a, Token::b)).unwrap();
	/// let b = parser.next().unwrap();
	///
	/// assert_eq!(a, Token::A("a".into(), span![1:1..1:2]));
	/// assert_eq!(b, Token::B("b".into(), span![1:2..1:3]));
	///
	/// # }
	/// ```
	///
	/// This method is (frankly) a sort of kludge to resolve a common ambiguity
	/// with language grammars that include both a right-shift operator (`>>`)
	/// and generic types with `<` and `>` as braces:
	///
	/// ```
	/// # #[macro_use] extern crate gramatika_macro;
	/// # fn main () {
	/// use gramatika::{Result, Parse, ParseStream, ParseStreamer, Span, Substr};
	///
	/// #[derive(Token, Lexer, Debug, PartialEq, Eq)]
	/// enum Token {
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	///
	///     #[pattern = r"&&?|\|\|?|--?|\+\+?|>>|<<"]
	///     #[pattern = "[=!<>]=?"]
	///     #[pattern = "[%*/~^]"]
	///     Operator(Substr, Span),
	/// }
	///
	/// #[derive(Debug)]
	/// struct Good {
	///     name: Token,
	///     generic: Option<Box<Good>>,
	/// }
	///
	/// #[derive(Debug)]
	/// struct Bad {
	///     name: Token,
	///     generic: Option<Box<Bad>>,
	/// }
	///
	/// impl Parse for Bad {
	///     type Stream = ParseStream<Token, Lexer>;
	///
	///     fn parse(input: &mut Self::Stream) -> Result<Self> {
	///         let mut result = Bad {
	///             name: input.consume_kind(TokenKind::Ident)?,
	///             generic: None,
	///         };
	///
	///         if input.check(operator![<]) {
	///             input.consume(operator![<])?;
	///             result.generic = Some(Box::new(input.parse()?));
	///             input.consume(operator![>])?;
	///         }
	///
	///         Ok(result)
	///     }
	/// }
	///
	/// impl Parse for Good {
	///     type Stream = ParseStream<Token, Lexer>;
	///
	///     fn parse(input: &mut Self::Stream) -> Result<Self> {
	///         let mut result = Good {
	///             name: input.consume_kind(TokenKind::Ident)?,
	///             generic: None,
	///         };
	///
	///         if input.check(operator![<]) {
	///             input.consume(operator![<])?;
	///             result.generic = Some(Box::new(input.parse()?));
	///
	///             if input.check(operator![>>]) {
	///                 input.split_next(1, (Token::operator, Token::operator))?;
	///             } else {
	///                 input.consume(operator![>])?;
	///             }
	///         }
	///
	///         Ok(result)
	///     }
	/// }
	///
	/// let input = "Foo<Bar<Baz>>";
	///
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	/// let bad = parser.parse::<Bad>();
	/// assert!(bad.is_err());
	///
	/// let err = bad.unwrap_err();
	/// let message = format!("{err}");
	/// assert_eq!(message, "
	/// ERROR: Expected `>`
	///   |
	/// 1 | Foo<Bar<Baz>>
	///   |            ^-
	/// ");
	///
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	/// let good = parser.parse::<Good>();
	/// assert!(good.is_ok());
	/// # }
	/// ```
	pub fn split_next(
		&mut self,
		split_at: usize,
		ctors: (TokenCtor<T>, TokenCtor<T>),
	) -> Result<T> {
		let Some(next) = self.peek.take().or_else(|| self.lexer.scan_token()) else {
			return Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: self.source(),
				span: None,
			});
		};

		let lexeme = next.lexeme();
		let span = next.span();

		if lexeme.len() <= split_at {
			return Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: self.source(),
				span: None,
			});
		}

		let (next_ctor, peek_ctor) = ctors;

		let peek = peek_ctor(
			lexeme.substr(split_at..),
			Span {
				start: Position {
					line: span.start.line,
					character: span.start.character + split_at,
				},
				end: span.end,
			},
		);

		let next = next_ctor(
			lexeme.substr(..split_at),
			Span {
				start: span.start,
				end: Position {
					line: span.end.line,
					character: span.end.character - split_at,
				},
			},
		);

		self.peek = Some(peek);
		self.tokens.push(next.clone());

		Ok(next)
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

	fn with_runtime_matcher<F>(mut self, matcher: F) -> Self
	where
		Self: Sized,
		F: Fn(&str) -> Option<(usize, <Self::Token as Token>::Kind)> + 'static,
	{
		self.lexer = self.lexer.with_runtime_matcher(matcher);
		self
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
