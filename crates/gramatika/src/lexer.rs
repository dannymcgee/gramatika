use std::fmt;

use arcstr::{ArcStr, Substr};

use crate::{Span, Spanned};

pub trait Lexer {
	type Output: Token;

	fn new(input: ArcStr) -> Self;
	fn source(&self) -> ArcStr;
	fn scan(&mut self) -> Vec<Self::Output>;
	fn scan_token(&mut self) -> Option<Self::Output>;
}

pub trait Token
where Self: Clone + Spanned
{
	type Kind: fmt::Debug + PartialEq;

	/// Returns the actual text content of a token.
	///
	/// ```
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
	/// ```
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
