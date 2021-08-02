use parse_framework::{Parse, TokenStream};

use super::*;
use crate::*;

#[derive(Debug, Parse)]
#[parse_token(super::Token<'a>)]
struct VarDecl<'a> {
	#[token(keyword![let])]
	pub storage: Token<'a>,

	#[token_kind(Ident)]
	pub ident: Token<'a>,

	#[token(operator![=])]
	pub eq: Token<'a>,

	#[token_kind(Literal)]
	pub expr: Token<'a>,

	#[token(punct![;])]
	pub semicolon: Token<'a>,
}

#[test]
fn var_decl() -> Result<(), String> {
	let input = "let foo = 42;";

	let mut lexer = Lexer::new(input);
	let mut stream = lexer.scan();

	let decl = stream.parse::<VarDecl>()?;

	eprintln!("{:#?}", decl);

	Ok(())
}
