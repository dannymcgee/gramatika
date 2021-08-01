use parse_framework::Span;

mod lexer;
mod parse;
mod tokens;

#[derive(Debug, PartialEq, Token, Lexer)]
pub(self) enum Token<'a> {
	#[pattern(r"^(let|var)")]
	Keyword(&'a str, Span),
	#[pattern(r"^[a-zA-Z_][a-zA-Z0-9_]*")]
	Ident(&'a str, Span),
	#[pattern(r"^[;:{}()\[\]]")]
	Punct(&'a str, Span),
	#[pattern(r"^[-+*/=]")]
	Operator(&'a str, Span),
	#[pattern(r"^[0-9]+")]
	Literal(&'a str, Span),
}
