use parse_framework::{Parse, TokenStream};

use super::*;
use crate::*;

#[derive(Debug)]
struct VarDecl<'a> {
	pub storage: Token<'a>,
	pub ident: Token<'a>,
	pub eq: Token<'a>,
	pub expr: Token<'a>,
	pub semicolon: Token<'a>,
}

impl<'a> Parse for VarDecl<'a> {
	type Token = super::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String> {
		use TokenKind::*;

		let storage = input.take(keyword![let])?;
		let ident = input.take_kind(Ident)?;
		let eq = input.take(operator![=])?;
		let expr = input.take_kind(Literal)?;
		let semicolon = input.take(punct![;])?;

		Ok(Self {
			storage,
			ident,
			eq,
			expr,
			semicolon,
		})
	}
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
