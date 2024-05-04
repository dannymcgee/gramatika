use gramatika::{span, Span, Substr};

extern crate gramatika;
#[macro_use]
extern crate gramatika_macro;

#[derive(Debug, Token, PartialEq)]
enum Token {
	Keyword(Substr, Span),
	Ident(Substr, Span),
	Punct(Substr, Span),
	Operator(Substr, Span),
	Literal(Substr, Span),
}

fn main() {
	// Constructor functions
	assert_eq!(
		Token::keyword("let".into(), span![1:1..1:4]),
		Token::Keyword("let".into(), span![1:1..1:4]),
	);
	assert_eq!(
		Token::ident("foo".into(), span![1:1..1:4]),
		Token::Ident("foo".into(), span![1:1..1:4]),
	);
	assert_eq!(
		Token::punct(";".into(), span![1:1..1:2]),
		Token::Punct(";".into(), span![1:1..1:2]),
	);
	assert_eq!(
		Token::operator("*".into(), span![1:1..1:2]),
		Token::Operator("*".into(), span![1:1..1:2]),
	);
	assert_eq!(
		Token::literal("42".into(), span![1:1..1:3]),
		Token::Literal("42".into(), span![1:1..1:3]),
	);

	// Macros
	#[rustfmt::skip]
	assert_eq!(
		keyword![let],
		Token::Keyword("let".into(), span![1:1..1:1]),
	);
	#[rustfmt::skip]
	assert_eq!(
		ident![foo],
		Token::Ident("foo".into(), span![1:1..1:1]),
	);
	#[rustfmt::skip]
	assert_eq!(
		punct![;],
		Token::Punct(";".into(), span![1:1..1:1]),
	);
	#[rustfmt::skip]
	assert_eq!(
		operator![*],
		Token::Operator("*".into(), span![1:1..1:1]),
	);
	#[rustfmt::skip]
	assert_eq!(
		literal!["42"],
		Token::Literal("42".into(), span![1:1..1:1]),
	);
}
