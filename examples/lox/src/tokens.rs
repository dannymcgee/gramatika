use std::fmt;

use gramatika::{DebugLisp, Span, Token as _};

#[derive(Clone, Copy, DebugLispToken, PartialEq, Token, Lexer)]
pub enum Token<'a> {
	#[pattern = r"(and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while)\b"]
	Keyword(&'a str, Span),

	#[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(&'a str, Span),

	#[pattern = r"[(){}]"]
	Brace(&'a str, Span),

	#[pattern = "[,.;]"]
	Punct(&'a str, Span),

	#[pattern = "[=!<>]=?"]
	#[pattern = "[-+*/]"]
	Operator(&'a str, Span),

	#[pattern = "[0-9]+"]
	NumLit(&'a str, Span),

	#[pattern = r#""[^"]+""#]
	StrLit(&'a str, Span),
}

impl<'a> fmt::Display for Token<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.lexeme())
	}
}

impl<'a> fmt::Debug for Token<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DebugLisp::fmt(self, f, 0)
	}
}
