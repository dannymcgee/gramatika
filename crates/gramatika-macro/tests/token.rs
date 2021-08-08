use gramatika::{span, Span};

#[macro_use]
extern crate gramatika;
#[macro_use]
extern crate gramatika_macro;

#[derive(Debug, Token, PartialEq)]
enum Token<'a> {
	Keyword(&'a str, Span),
	Ident(&'a str, Span),
	Punct(&'a str, Span),
	Operator(&'a str, Span),
	Literal(&'a str, Span),
}

fn main() {
	// Constructor functions
	assert_eq!(
		Token::keyword("let", span![0:0...0:3]),
		Token::Keyword("let", span![0:0...0:3]),
	);
	assert_eq!(
		Token::ident("foo", span![0:0...0:3]),
		Token::Ident("foo", span![0:0...0:3]),
	);
	assert_eq!(
		Token::punct(";", span![0:0...0:1]),
		Token::Punct(";", span![0:0...0:1]),
	);
	assert_eq!(
		Token::operator("*", span![0:0...0:1]),
		Token::Operator("*", span![0:0...0:1]),
	);
	assert_eq!(
		Token::literal("42", span![0:0...0:2]),
		Token::Literal("42", span![0:0...0:2]),
	);

	// Macros
	#[rustfmt::skip]
	assert_eq!(
		keyword![let],
		Token::Keyword("let", span![0:0...0:0]),
	);
	#[rustfmt::skip]
	assert_eq!(
		ident![foo],
		Token::Ident("foo", span![0:0...0:0]),
	);
	#[rustfmt::skip]
	assert_eq!(
		punct![;],
		Token::Punct(";", span![0:0...0:0]),
	);
	#[rustfmt::skip]
	assert_eq!(
		operator![*],
		Token::Operator("*", span![0:0...0:0]),
	);
	#[rustfmt::skip]
	assert_eq!(
		literal!["42"],
		Token::Literal("42", span![0:0...0:0]),
	);
}
