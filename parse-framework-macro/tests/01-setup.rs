use parse_framework::Span;

#[macro_use]
extern crate parse_framework_macro;

// Verify `#[derive(Token)]` can be invoked without exploding

#[derive(Token)]
pub enum Token<'a> {
	Ident(&'a str, Span),
}

fn main() {}
