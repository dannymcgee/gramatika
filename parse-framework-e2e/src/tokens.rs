use std::fmt;

use parse_framework::{Span, Token as _};

#[derive(Clone, Copy, Debug, PartialEq, Token, Lexer)]
pub enum Token<'a> {
	#[pattern(r"^(and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while)\b")]
	Keyword(&'a str, Span),

	#[pattern(r"^[a-zA-Z_][a-zA-Z0-9_]*")]
	Ident(&'a str, Span),

	#[pattern(r"^[(){}]")]
	Brace(&'a str, Span),

	#[pattern(r"^[,.;]")]
	Punct(&'a str, Span),

	#[pattern(r"^([=!<>]=?|[-+*/])")]
	Operator(&'a str, Span),

	#[pattern(r"^[0-9]+")]
	NumLit(&'a str, Span),

	#[pattern(r#"^"[^"]+""#)]
	StrLit(&'a str, Span),
}

impl<'a> fmt::Display for Token<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.lexeme())
	}
}
