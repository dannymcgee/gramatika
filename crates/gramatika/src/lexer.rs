// TODO
//! ```ignore
//! # #[macro_use]
//! # extern crate gramatika;
//! # fn main() {
//! use gramatika::{Span, Substr};
//!
//! #[derive(PartialEq, Token, Lexer)]
//! enum Token {
//!     #[discard]
//!     #[pattern = "//.*"]
//!     LineComment(Substr, Span),
//!
//!     #[discard]
//!     #[multiline]
//!     #[pattern = r"/\*.*?\*/"]
//!     BlockComment(Substr, Span),
//!
//!     #[subset_of(Ident)]
//!     #[pattern = "if|else|switch|case|break|for|while|var|print"]
//!     Keyword(Substr, Span),
//!
//!     #[pattern = "[a-zA-Z_][a-zA-Z_0-9]*"]
//!     Ident(Substr, Span),
//!
//!     #[pattern = r"[(){}\[\]]"]
//!     Brace(Substr, Span),
//!
//!     #[pattern = "[,.;]"]
//!     Punct(Substr, Span),
//!
//!     #[pattern = "[=!<>]=?"]
//!     #[pattern = "[-+*/]"]
//!     Operator(Substr, Span),
//!
//!     #[pattern = "(0[xb])?[0-9A-Fa-f][0-9A-Fa-f.]*"]
//!     NumLiteral(Substr, Span),
//!
//!     #[pattern = r#""[^"]+""#]
//!     StrLiteral(Substr, Span),
//! }
//! # }
//! ```
//! That may seem unassuming, but Gramatika is generating _a lot_ of code from
//! those `#[derive(...)]` attributes. Let's take a high-level tour of that
//! generated code piece by piece.
//!
//! ### `#[derive(Token)]`
//!
//! This implements the `gramatika::Token` trait, along with some additional
//! helpers that make it easier to actually use those tokens in your parser and
//! applications.
//!
//! Each member should be defined as a tuple variant with a `(Substr, Span)`.
//!
//! * The [`Substr`] portion (which we'll refer to as the _lexeme_) is an atomic
//!   reference-counted view into the original source string. These can be
//!   `clone`d for very little cost, because only the pointer to the original
//!   string is copied, not the actual string itself.
//!
//!   ```ignore
//!   # use gramatika::{ArcStr, Substr};
//!   let source = ArcStr::from("foo bar baz");
//!   {
//!       let foo = source.substr(..3);
//!       let baz = source.substr(8..);
//!
//!       assert_eq!(foo, "foo");
//!       assert_eq!(baz, "baz");
//!       assert!(ArcStr::ptr_eq(foo.parent(), baz.parent()));
//!       assert_eq!(ArcStr::strong_count(&source), Some(3));
//!   }
//!   assert_eq!(ArcStr::strong_count(&source), Some(1));
//!   ```
//!
//! * The [`Span`] indicates the token's location in the original source
//!   document by line and character number.
//!
//!   It's important to note that while the actual values stored in the `Span`
//!   are zero-indexed, printing the `Span` with the `Debug` trait will display
//!   _one-indexed_ values to match the conventions of most code and text
//!   editors.
//!
//!   ```ignore
//!   # use gramatika::Span;
//!   let span = Span::new((0, 0), (0, 4));
//!   let printed = format!("{span:?}");
//!   assert_eq!(printed, "1:1..1:5");
//!   ```
//!
//! ```ignore
//! #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
//! pub enum TokenKind {
//!     LineComment,
//!     BlockComment,
//!     Keyword,
//!     // ...
//!     StrLit,
//! }
//!
//! impl Token {
//!     pub fn as_inner(&self) -> (Substr, Span) { /* ... */ }
//!     // Constructor functions
//!     pub fn keyword(lexeme: Substr, span: Span) -> Self { /* ... */ }
//!     // ...
//!     pub fn str_lit(lexeme: Substr, span: Span) -> Self { /* ... */ }
//! }
//!
//! macro_rules! keyword { /* ... */ }
//! pub(crate) use keyword;
//! // ...
//! macro_rules! str_lit { /* ... */ }
//! pub(crate) use str_lit;
//!
//! impl Clone for Token { /* ... */ }
//!
//! impl gramatika::Token for Token {
//!     type Kind = TokenKind;
//!
//!     fn lexeme(&self) -> Substr { /* ... */ }
//!     fn kind(&self) -> TokenKind { /* ... */ }
//!     fn as_matchable(&self) -> (Self::Kind, &str, Span) { /* ... */ }
//! }
//!
//! impl gramatika::Spanned for Token {
//!     fn span(&self) -> Span { /* ... */ }
//! }
//!
//! impl gramatika::DebugLisp for Token {
//!     // ...
//! }
//! ```

use std::fmt;

use arcstr::{ArcStr, Substr};

use crate::{Span, Spanned};

pub trait Lexer {
	type Output: Token;

	fn new(input: ArcStr) -> Self;

	#[allow(unused_variables)]
	fn with_runtime_matcher<F>(self, matcher: F) -> Self
	where
		Self: Sized,
		F: Fn(&str) -> Option<(usize, <Self::Output as Token>::Kind)> + 'static,
	{
		self
	}

	fn source(&self) -> ArcStr;
	fn scan_token(&mut self) -> Option<Self::Output>;
	fn scan(&mut self) -> Vec<Self::Output> {
		let mut result = vec![];
		while let Some(token) = self.scan_token() {
			result.push(token);
		}
		result
	}
}

pub trait Token
where Self: Clone + Spanned
{
	type Kind: fmt::Debug + PartialEq;

	/// Returns the actual text content of a token.
	///
	/// ```ignore
	/// #[derive(Token, Lexer)]
	/// enum Token {
	///     #[subset_of(Ident)]
	///     #[pattern = "var"]
	///     Keyword(Substr, Span),
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	///     #[pattern = "="]
	///     Operator(Substr, Span),
	///     #[pattern = ";"]
	///     Punct(Substr, Span),
	/// }
	///
	/// let src = "var the_answer = 42;";
	/// let tokens = Lexer::new(src.into()).scan();
	///
	/// assert_eq!(tokens[1].lexeme(), literal_substr!("the_answer"));
	/// ```
	fn lexeme(&self) -> Substr;

	/// Returns the [`Kind`] of this token. Used in [`ParseStreamer`] methods like
	/// [`check_kind`] and [`consume_kind`]. Effectively a more user-friendly version of
	/// [`std::mem::discriminant`].
	///
	/// [`Kind`]: Token::Kind
	/// [`ParseStreamer`]: crate::parse::ParseStreamer
	/// [`check_kind`]: crate::parse::ParseStreamer::check_kind
	/// [`consume_kind`]: crate::parse::ParseStreamer::consume_kind
	fn kind(&self) -> Self::Kind;

	/// Provides a convenient API for matching on a token's constituent parts, with
	/// string-literal patterns for its [`lexeme`].
	///
	/// [`lexeme`]: Token::lexeme
	/// ```ignore
	/// impl Parse for Stmt {
	///     type Stream = MyStream;
	///     fn parse(input: &mut Self::Stream) -> Result<Self> {
	///         match input.next() {
	///             Some(token) => match token.as_matchable() {
	///                 (TokenKind::Keyword, "class" | "fun" | "var", _) => {
	///                     Ok(Stmt::Decl(input.parse()?))
	///                 }
	///                 (TokenKind::Keyword, "if", _) => {
	///                     Ok(Stmt::If(input.parse()?))
	///                 }
	///                 (TokenKind::Keyword, "for", _) => {
	///                     Ok(Stmt::For(input.parse()?))
	///                 }
	///                 _ => Err(SpannedError {
	///                     message:
	///                         "Expected `class`, `fun`, `var`, `if`, or `for`.".into(),
	///                     source: input.source(),
	///                     span: Some(token.span()),
	///                 }),
	///             }
	///             None => Err(SpannedError {
	///                 message: "Unexpected end of input.".into(),
	///                 source: input.source(),
	///                 span: input.prev().map(|token| token.span()),
	///             }),
	///         }
	///     }
	/// }
	/// ```
	fn as_matchable(&self) -> (Self::Kind, &str, Span);
}
