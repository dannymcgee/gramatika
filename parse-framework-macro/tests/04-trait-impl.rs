use parse_framework::{Span, Token as TokenTrait};

#[macro_use]
extern crate parse_framework_macro;

#[derive(Debug, Token)]
enum Token<'a> {
	Ident(&'a str, Span),
}

fn main() {
	let token = Token::ident("foo", Span::new((0, 0), (0, 3)));
	let lexeme = token.lexeme();
	let span = token.span();

	eprintln!("lexeme: `{}`", lexeme);
	eprintln!("span: {:?}", span);
}
