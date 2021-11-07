//! NOTE: This crate only exists to debug macros.
//! Run `cargo expand -p expand` to check the output of the macros used here.

#[macro_use]
extern crate gramatika;

use gramatika::{Lexer as _, Span, Substr};

#[derive(Debug, Token, Lexer, PartialEq)]
enum Token {
	#[subset_of(Ident)]
	#[pattern = "and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while"]
	Keyword(Substr, Span),

	#[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(Substr, Span),

	#[pattern = r"[;:{}()\[\]]"]
	Punct(Substr, Span),

	#[pattern = "[-+*/=]"]
	Operator(Substr, Span),

	#[pattern = "[0-9]+"]
	Literal(Substr, Span),
}

fn main() {
	let input = "let foo = 2 + 2;";
	let mut lexer = Lexer::new(input.into());
	let tokens = lexer.scan();

	let expected = vec![
		Token::keyword("let".into(), span![0:0...0:3]),
		Token::ident("foo".into(), span![0:4...0:7]),
		Token::operator("=".into(), span![0:8...0:9]),
		Token::literal("2".into(), span![0:10...0:11]),
		Token::operator("+".into(), span![0:12...0:13]),
		Token::literal("2".into(), span![0:14...0:15]),
		Token::punct(";".into(), span![0:15...0:16]),
	];

	assert_eq!(tokens, expected);
}
