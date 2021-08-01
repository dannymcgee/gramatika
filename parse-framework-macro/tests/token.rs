use parse_framework::{span, Span};

#[macro_use]
extern crate parse_framework_macro;

#[derive(Debug, Token)]
enum Token<'a> {
	Keyword(&'a str, Span),
	Ident(&'a str, Span),
	Punct(&'a str, Span),
	Operator(&'a str, Span),
	Literal(&'a str, Span),
}

fn main() {
	let _ = Token::keyword("let", span![0:0...0:3]);
	let _ = Token::ident("foo", span![0:0...0:3]);
	let _ = Token::punct(";", span![0:0...0:1]);
	let _ = Token::operator("*", span![0:0...0:1]);
	let _ = Token::literal("42", span![0:0...0:2]);

	let keyword = keyword![let];
	let ident = ident![foo];
	let punct = punct![;];
	let operator = operator![*];
	let literal = literal![42];

	eprintln!("{:?}", keyword);
	eprintln!("{:?}", ident);
	eprintln!("{:?}", punct);
	eprintln!("{:?}", operator);
	eprintln!("{:?}", literal);
}
