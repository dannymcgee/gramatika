use core::fmt;

use arcstr::Substr;

use crate::{Lexer, Position, Result, Span, Spanned, SpannedError, Token};

pub type TokenCtor<T> = fn(Substr, Span) -> T;

/// A trait to be implemented by any type that can be parsed using the
/// [`ParseStreamer`] interface.
pub trait Parse
where Self: Sized
{
	type Stream: ParseStreamer;

	fn parse(input: &mut Self::Stream) -> Result<Self>;
}

/// A user-friendly interface for implementing a hand-written LL(1) or recursive
/// descent parser with backtracking.
///
/// It serves as the `input` parameter for the [`Parse`] trait's
/// [`parse`](Parse::parse) method, allowing the implementation of a full syntax
/// tree parser to be broken up into discrete [`Parse`] implementations for each
/// node in the tree.
pub trait ParseStreamer {
	type Token: Token + Spanned;

	/// Experimental
	#[doc(hidden)]
	#[allow(unused_variables)]
	fn with_runtime_matcher<F>(self, matcher: F) -> Self
	where
		Self: Sized,
		F: Fn(&str) -> Option<(usize, <Self::Token as Token>::Kind)> + 'static,
	{
		self
	}

	/// Provides a more convenient API for parsing other implementers of the
	/// [`Parse`] trait.
	///
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug)]
	/// enum Token {
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	/// }
	///
	/// #[derive(Debug)]
	/// struct IdentExpr(Token);
	///
	/// impl Parse for IdentExpr {
	///     // ...
	/// #     type Stream = ParseStream<Token, Lexer>;
	/// #
	/// #     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
	/// #         Ok(Self(input.consume_kind(TokenKind::Ident)?))
	/// #     }
	/// }
	///
	/// let input = "foo bar baz";
	/// let mut parser = ParseStream::from(input);
	///
	/// let foo = IdentExpr::parse(&mut parser)?;
	/// let bar: IdentExpr = parser.parse()?;
	/// let baz = parser.parse::<IdentExpr>()?;
	///
	/// # Ok(())
	/// # }
	/// ```
	fn parse<P>(&mut self) -> Result<P>
	where P: Parse<Stream = Self> {
		P::parse(self)
	}

	/// Returns `true` when there are no more tokens in the stream.
	fn is_empty(&mut self) -> bool;

	/// Returns a [`Some`] reference to the next [`Token`] in the stream without
	/// advancing the iterator, or [`None`] if the stream is empty.
	///
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug)]
	/// enum Token {
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	/// }
	///
	/// let input = "foo";
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	/// assert!(parser.peek().is_some());
	///
	/// match parser.peek() {
	///     Some(peeked) => {
	///         assert!(matches!(peeked.as_matchable(), (TokenKind::Ident, "foo", _)));
	///     },
	///     None => unreachable!(),
	/// }
	///
	/// let _ = parser.consume_kind(TokenKind::Ident)?;
	/// assert!(parser.peek().is_none());
	///
	/// # Ok(())
	/// # }
	/// ```
	fn peek(&mut self) -> Option<&Self::Token>;

	/// Returns a [`Some`] reference to the last token consumed by the iterator.
	/// Returns [`None`] if the source string contains no tokens, or if no tokens
	/// have been consumed yet.
	///
	/// Underlying data access is `O(1)` in the [crate-provided implementation].
	///
	/// [crate-provided implementation]: ParseStream
	///
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug, PartialEq)]
	/// enum Token {
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	/// }
	///
	/// let input = "foo";
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	/// assert!(parser.prev().is_none());
	///
	/// let foo = parser.consume_kind(TokenKind::Ident)?;
	/// assert!(parser.prev().is_some());
	/// assert_eq!(parser.prev().unwrap(), &foo);
	///
	/// # Ok(())
	/// # }
	/// ```
	fn prev(&mut self) -> Option<&Self::Token>;

	/// Indicates whether the next [`Token`] in the stream matches the given
	/// [`Kind`], without advancing the iterator.
	///
	/// [`Kind`]: Token::Kind
	///
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug, PartialEq)]
	/// enum Token {
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	///     #[pattern = r"\S+"]
	///     Unrecognized(Substr, Span),
	/// }
	///
	/// let input = "foo";
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	/// assert_eq!(
	///     parser.check_kind(TokenKind::Unrecognized),
	///     false,
	/// );
	/// assert_eq!(
	///     parser.check_kind(TokenKind::Ident),
	///     true,
	/// );
	///
	/// # Ok(())
	/// # }
	/// ```
	fn check_kind(&mut self, kind: <Self::Token as Token>::Kind) -> bool;

	/// Indicates whether the next [`Token`] in the stream matches the given
	/// argument by comparing their [`Kind`]s and [`lexeme`]s. Does not advance
	/// the iterator.
	///
	/// [`Kind`]: Token::Kind
	/// [`lexeme`]: Token::lexeme
	///
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug, PartialEq)]
	/// enum Token {
	///     #[pattern = "[-+*/=<>]"]
	///     Operator(Substr, Span),
	/// }
	///
	/// let input = "=";
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	/// assert_eq!(
	///     parser.check(operator![>]),
	///     false,
	/// );
	/// assert_eq!(
	///     parser.check(operator![=]),
	///     true,
	/// );
	///
	/// # Ok(())
	/// # }
	/// ```
	fn check(&mut self, compare: Self::Token) -> bool;

	/// Advances the iterator, returning [`Ok`] with the next [`Token`] if it
	/// matches the given argument by comparing their [`Kind`]s and [`lexeme`]s.
	/// Otherwise returns a contextual [`Err`]`(`[`SpannedError`]`)`.
	///
	/// [`Kind`]: Token::Kind
	/// [`lexeme`]: Token::lexeme
	///
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug, PartialEq)]
	/// enum Token {
	///     #[pattern = "[-+*/=<>]"]
	///     Operator(Substr, Span),
	/// }
	///
	/// let input = "=<";
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	///
	/// let eq = parser.consume(operator![=]);
	/// assert!(eq.is_ok());
	///
	/// let gt = parser.consume(operator![>]);
	/// assert!(gt.is_err());
	///
	/// let error = gt.unwrap_err();
	/// assert_eq!(format!("{error}"), r#"
	/// ERROR: Expected `>`
	///   |
	/// 1 | =<
	///   |  ^
	/// "#);
	///
	/// # Ok(())
	/// # }
	/// ```
	fn consume(&mut self, compare: Self::Token) -> Result<Self::Token>;

	/// Advances the iterator, returning [`Ok`] with the next [`Token`] if it
	/// matches the given [`Kind`]. Otherwise returns a contextual
	/// [`Err`]`(`[`SpannedError`]`)`.
	///
	/// [`Kind`]: Token::Kind
	///
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug, PartialEq)]
	/// enum Token {
	///     #[pattern = "[-+*/<>]"]
	///     Operator(Substr, Span),
	///
	///     #[pattern = "[0-9]+"]
	///     Number(Substr, Span),
	/// }
	///
	/// let input = "2++";
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	///
	/// let lhs = parser.consume_kind(TokenKind::Number);
	/// assert!(lhs.is_ok());
	///
	/// let op = parser.consume_kind(TokenKind::Operator);
	/// assert!(op.is_ok());
	///
	/// let rhs = parser.consume_kind(TokenKind::Number);
	/// assert!(rhs.is_err());
	///
	/// let error = rhs.unwrap_err();
	/// assert_eq!(format!("{error}"), r#"
	/// ERROR: Expected Number, found Operator
	///   |
	/// 1 | 2++
	///   |   ^
	/// "#);
	///
	/// # Ok(())
	/// # }
	/// ```
	fn consume_kind(&mut self, kind: <Self::Token as Token>::Kind)
		-> Result<Self::Token>;

	/// Advances the iterator, consuming the next token while converting it into
	/// a different [`Kind`] using the provided `convert` function.
	///
	/// [`Kind`]: Token::Kind
	///
	/// # Example
	///
	/// This is primarily useful for "upgrading" less specific token variants
	/// into more specific subsets of those variants at parse-time.
	///
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// #     SpannedError,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug, PartialEq)]
	/// enum Token {
	///     #[subset_of(Ident)]
	///     #[pattern = "func|struct"]
	///     Storage(Substr, Span),
	///
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	///
	///     #[pattern = r"[(){}\[\]]"]
	///     Brace(Substr, Span),
	///
	///     StructName(Substr, Span),
	///     FuncName(Substr, Span),
	/// }
	///
	/// let input = r#"
	///     struct Foo {}
	///     func bar() {}
	/// "#;
	///
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	///
	/// while let Some(token) = parser.next() {
	///     use TokenKind::*;
	///
	///     match token.as_matchable() {
	///         (Storage, "struct", _) => {
	///             let ident = parser.consume_as(Ident, Token::struct_name)?;
	///             assert_eq!(ident.kind(), StructName);
	///             parser.consume(brace!("{"))?;
	///             parser.consume(brace!("}"))?;
	///         }
	///         (Storage, "func", _) => {
	///             let ident = parser.consume_as(Ident, Token::func_name)?;
	///             assert_eq!(ident.kind(), FuncName);
	///             parser.consume(brace!("("))?;
	///             parser.consume(brace!(")"))?;
	///             parser.consume(brace!("{"))?;
	///             parser.consume(brace!("}"))?;
	///         }
	///         (_, _, span) => {
	///             return Err(SpannedError {
	///                 message: "Expected `struct` or `func`".into(),
	///                 source: parser.source(),
	///                 span: Some(span),
	///             });
	///         }
	///     }
	/// }
	///
	/// # Ok(())
	/// # }
	/// ```
	fn consume_as(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token>;

	/// Similar to [`consume_as`], but retroactively upgrades the last token
	/// consumed by the parser.
	///
	/// [`consume_as`]: ParseStreamer::consume_as
	///
	/// # Example
	///
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// #     SpannedError,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug, PartialEq)]
	/// enum Token {
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	///
	///     #[pattern = "[()]"]
	///     Brace(Substr, Span),
	///
	///     #[pattern = "="]
	///     Eq(Substr, Span),
	///
	///     FuncName(Substr, Span),
	/// }
	///
	/// let input = "foo = bar()";
	///
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	///
	/// let lhs = parser.consume_kind(TokenKind::Ident)?;
	/// let eq = parser.consume(eq![=])?;
	/// let mut rhs = parser.consume_kind(TokenKind::Ident)?;
	///
	/// match parser.peek() {
	///     Some(peeked) if matches!(peeked.as_matchable(), (TokenKind::Brace, "(", _)) => {
	///         rhs = parser.upgrade_last(TokenKind::Ident, Token::func_name)?;
	///     }
	///     _ => {}
	/// }
	///
	/// assert_eq!(rhs.kind(), TokenKind::FuncName);
	///
	/// # Ok(())
	/// # }
	/// ```
	fn upgrade_last(
		&mut self,
		kind: <Self::Token as Token>::Kind,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token>;

	/// Similar to [`upgrade_last`], but retroactively upgrades any arbitrary
	/// token the parser has previously consumed.
	///
	/// # Warning
	///
	/// The default implementation of this operation runs in `O(n)` time, where
	/// `n` is the number of tokens consumed so far. Prefer to use
	/// [`upgrade_last`] when possible.
	///
	/// # Panics
	///
	/// The default implementation will panic if the provided `token` cannot be
	/// found in the parser's buffer of previously consumed tokens.
	///
	/// [`upgrade_last`]: ParseStreamer::upgrade_last
	///
	/// # Example
	/// ```
	/// # #[macro_use]
	/// # extern crate gramatika;
	/// #
	/// # use gramatika::{
	/// #     Parse, ParseStream, ParseStreamer,
	/// #     Substr, Span, Token as _,
	/// #     SpannedError,
	/// # };
	/// #
	/// # fn main() -> gramatika::Result<()> {
	/// // ...
	/// #[derive(Token, Lexer, Debug, PartialEq)]
	/// enum Token {
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	///
	///     #[pattern = "[()]"]
	///     Brace(Substr, Span),
	///
	///     #[pattern = "="]
	///     Eq(Substr, Span),
	///
	///     FuncName(Substr, Span),
	/// }
	///
	/// let input = "foo = bar()";
	///
	/// let mut parser = ParseStream::<Token, Lexer>::from(input);
	///
	/// let lhs = parser.consume_kind(TokenKind::Ident)?;
	/// let eq = parser.consume(eq![=])?;
	/// let mut rhs = parser.consume_kind(TokenKind::Ident)?;
	///
	/// match parser.peek() {
	///     Some(peeked) if matches!(peeked.as_matchable(), (TokenKind::Brace, "(", _)) => {
	///         rhs = parser.upgrade(rhs, Token::func_name)?;
	///     }
	///     _ => {}
	/// }
	///
	/// assert_eq!(rhs.kind(), TokenKind::FuncName);
	///
	/// # Ok(())
	/// # }
	/// ```
	fn upgrade(
		&mut self,
		token: Self::Token,
		convert: TokenCtor<Self::Token>,
	) -> Result<Self::Token>;

	/// Advances the iterator, ignoring the next [`Token`].
	fn discard(&mut self);
}

/// A concrete implementation of the [`ParseStreamer`] interface.
///
/// For most applications, it should be sufficient to use this type as the
/// "engine" for your parser, by deriving [`Token`] and [`Lexer`] for an enum
/// type representing your language's tokens[^note], and using
/// `ParseStream<T, L>` as the [`Stream`] type for your syntax tree's [`Parse`]
/// implementations, where `T` is your concrete token type and `L` is your
/// generated lexer.
///
/// See the [crate-level documentation] and the [`lexer`](crate::lexer)
/// documentation for examples and (much) more detail, and see the
/// [`ParseStreamer`] documentation for an overview of this type's public API.
///
/// [`Stream`]: Parse::Stream
/// [crate-level documentation]: crate
pub struct ParseStream<T, L>
where
	T: Token + Spanned,
	L: Lexer<Output = T>,
{
	input: Substr,
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

	pub fn source(&self) -> Substr {
		self.input.clone()
	}

	pub fn into_inner(self) -> (Substr, Vec<T>) {
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
					let span = token.span();
					self.tokens.push(token);

					Some(Err(SpannedError {
						message: format!("Expected {:?}", kind),
						source: self.source(),
						span: Some(span),
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
					let span = token.span();
					self.tokens.push(token);

					Err(SpannedError {
						message: format!("Expected {:?}", kind),
						source: self.source(),
						span: Some(span),
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
			.rev()
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
	S: Into<Substr>,
	T: Token + Spanned,
	L: Lexer<Output = T>,
{
	fn from(input: S) -> Self {
		let input = input.into();
		let lexer = L::new(input.clone());

		Self {
			input,
			lexer,
			peek: None,
			tokens: vec![],
		}
	}
}
