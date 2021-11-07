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
		Token::keyword("let".into(), span![0:0...0:3]),
		Token::Keyword("let".into(), span![0:0...0:3]),
	);
	assert_eq!(
		Token::ident("foo".into(), span![0:0...0:3]),
		Token::Ident("foo".into(), span![0:0...0:3]),
	);
	assert_eq!(
		Token::punct(";".into(), span![0:0...0:1]),
		Token::Punct(";".into(), span![0:0...0:1]),
	);
	assert_eq!(
		Token::operator("*".into(), span![0:0...0:1]),
		Token::Operator("*".into(), span![0:0...0:1]),
	);
	assert_eq!(
		Token::literal("42".into(), span![0:0...0:2]),
		Token::Literal("42".into(), span![0:0...0:2]),
	);

	// Macros
	#[rustfmt::skip]
	assert_eq!(
		keyword![let],
		Token::Keyword("let".into(), span![0:0...0:0]),
	);
	#[rustfmt::skip]
	assert_eq!(
		ident![foo],
		Token::Ident("foo".into(), span![0:0...0:0]),
	);
	#[rustfmt::skip]
	assert_eq!(
		punct![;],
		Token::Punct(";".into(), span![0:0...0:0]),
	);
	#[rustfmt::skip]
	assert_eq!(
		operator![*],
		Token::Operator("*".into(), span![0:0...0:0]),
	);
	#[rustfmt::skip]
	assert_eq!(
		literal!["42"],
		Token::Literal("42".into(), span![0:0...0:0]),
	);
}
