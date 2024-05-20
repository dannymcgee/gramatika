use std::fmt;

use gramatika::{DebugLisp, Span, Substr, Token as _};

#[derive(DebugLispToken, PartialEq, Token)]
pub enum Token {
	#[discard]
	#[pattern = "//.*"]
	LineComment(Substr, Span),

	#[discard]
	#[multiline]
	#[pattern = r"/\*.*?\*/"]
	BlockComment(Substr, Span),

	#[subset_of(Ident)]
	#[pattern = "and|class|else|false|for|fun|if|nil|or|print|return|super|this|true|var|while"]
	Keyword(Substr, Span),

	#[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(Substr, Span),

	#[pattern = r"[(){}]"]
	Brace(Substr, Span),

	#[pattern = "[,.;]"]
	Punct(Substr, Span),

	#[pattern = "[=!<>]=?"]
	#[pattern = "[-+*/]"]
	Operator(Substr, Span),

	#[pattern = "[0-9]+"]
	NumLit(Substr, Span),

	#[pattern = r#""[^"]*""#]
	StrLit(Substr, Span),
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.lexeme())
	}
}

impl fmt::Debug for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DebugLisp::fmt(self, f, 0)
	}
}
