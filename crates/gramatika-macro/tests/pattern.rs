#[allow(unused_imports)]
#[macro_use]
extern crate gramatika_macro;
#[macro_use]
extern crate gramatika;

use gramatika::{Span, Substr};

#[allow(dead_code)]
#[derive(Debug, Token)]
enum Token {
	#[subset_of(Ident)]
	#[pattern = "let|var|if|else|elsif|for|while|return"]
	Keyword(Substr, Span),

	#[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(Substr, Span),

	#[pattern = r"[;:{}()\[\]]"]
	Punct(Substr, Span),

	#[pattern = "[=!<>]=?"]
	#[pattern = "[-+*/]"]
	Operator(Substr, Span),

	#[pattern = "[0-9]+"]
	Literal(Substr, Span),

	#[pattern = r#""[^"]*""#]
	StrLiteral(Substr, Span),

	NoPattern(Substr, Span),
}

fn main() {
	assert!(Token::match_keyword("let").is_some());
	assert!(Token::match_ident("foo").is_some());
	assert!(Token::match_punct(";").is_some());
	assert!(Token::match_operator("*").is_some());
	assert!(Token::match_operator("=").is_some());
	assert!(Token::match_operator("!=").is_some());
	assert!(Token::match_literal("42").is_some());
}
