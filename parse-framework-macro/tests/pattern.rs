#[macro_use]
extern crate parse_framework_macro;
#[macro_use]
extern crate parse_framework;

use parse_framework::Span;

#[allow(dead_code)]
#[derive(Debug, Token)]
enum Token<'a> {
	#[pattern(r"^(let|var)")]
	Keyword(&'a str, Span),
	#[pattern(r"^[a-zA-Z_][a-zA-Z0-9_]*")]
	Ident(&'a str, Span),
	#[pattern(r"^[;:{}()\[\]]")]
	Punct(&'a str, Span),
	#[pattern(r"^[-+*/=]")]
	Operator(&'a str, Span),
	#[pattern(r"^[0-9]+")]
	Literal(&'a str, Span),
}

fn main() {
	if let Some(m) = Token::match_keyword("let") {
		eprintln!(
			"{:?}",
			Token::keyword(m.as_str(), Span::new((0, m.start()), (0, m.end())))
		);
	}
	if let Some(m) = Token::match_ident("foo") {
		eprintln!(
			"{:?}",
			Token::ident(m.as_str(), Span::new((0, m.start()), (0, m.end())))
		);
	}
	if let Some(m) = Token::match_punct(";") {
		eprintln!(
			"{:?}",
			Token::punct(m.as_str(), Span::new((0, m.start()), (0, m.end())))
		);
	}
	if let Some(m) = Token::match_operator("*") {
		eprintln!(
			"{:?}",
			Token::operator(m.as_str(), Span::new((0, m.start()), (0, m.end())))
		);
	}
	if let Some(m) = Token::match_literal("42") {
		eprintln!(
			"{:?}",
			Token::literal(m.as_str(), Span::new((0, m.start()), (0, m.end())))
		);
	}
}
