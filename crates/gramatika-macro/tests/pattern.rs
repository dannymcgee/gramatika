#[allow(unused_imports)]
#[macro_use]
extern crate gramatika_macro;
#[macro_use]
extern crate gramatika;

use gramatika::Span;

#[allow(dead_code)]
#[derive(Debug, Token)]
enum Token<'a> {
	#[pattern = "let|var|if|else|elsif|for|while|return"]
	Keyword(&'a str, Span),

	#[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(&'a str, Span),

	#[pattern = r"[;:{}()\[\]]"]
	Punct(&'a str, Span),

	#[pattern = "[=!<>]=?"]
	#[pattern = "[-+*/]"]
	Operator(&'a str, Span),

	#[pattern = "[0-9]+"]
	Literal(&'a str, Span),

	#[pattern = r#""[^"]*""#]
	StrLiteral(&'a str, Span),
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
