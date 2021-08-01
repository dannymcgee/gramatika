use parse_framework::Span;

#[macro_use]
extern crate parse_framework_macro;

/// Macro should generate an `as_inner(&self)` method which returns the lexeme and span
/// without having to match on the enum and unwrap its contents.
///
/// Expected output:
/// ```
/// impl<'a> Token<'a> {
///     pub fn as_inner(&'a self) -> (&'a str, Span) {
///         match self {
///             Self::Ident(lexeme, span) => (*lexeme, *span),
///         }
///     }
/// }
/// ```

// --

#[derive(Debug, Token)]
enum Token<'a> {
	Ident(&'a str, Span),
}

fn main() {
	let token = Token::ident("foo", Span::new((0, 0), (0, 3)));
	let (lexeme, span) = token.as_inner();
	eprintln!("lexeme: `{}`", lexeme);
	eprintln!("span: {:?}", span);
}
