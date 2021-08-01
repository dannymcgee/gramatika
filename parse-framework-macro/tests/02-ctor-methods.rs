use parse_framework::Span;

#[macro_use]
extern crate parse_framework_macro;

/// Macro should generate constructor methods for each enum variant.
///
/// Expected output:
/// ```
/// impl<'a> Token<'a> {
///     pub fn ident(arg0: &'a str, arg1: Span) -> Self {
///         Self::Ident(arg0, arg1)
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
	eprintln!("token: {:?}", token);
}
