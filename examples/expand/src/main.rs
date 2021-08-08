//! NOTE: This crate only exists to debug macros.
//! Run `cargo expand -p expand` to check the output of the macros used here.

use gramatika::Span;

#[macro_use]
extern crate gramatika;

#[allow(dead_code)]
#[derive(Debug, Token)]
enum Token<'a> {
	#[pattern = "[=!<>]=?"]
	#[pattern = "[-+*/]"]
	Operator(&'a str, Span),

	#[pattern = "let|var|if|else|elsif|for|while|return"]
	Keyword(&'a str, Span),

	#[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(&'a str, Span),

	#[pattern = r"[;:{}()\[\]]"]
	Punct(&'a str, Span),

	#[pattern = "[0-9]+"]
	Literal(&'a str, Span),

	#[pattern = r#""[^"]*""#]
	StrLiteral(&'a str, Span),
}

fn main() {}
