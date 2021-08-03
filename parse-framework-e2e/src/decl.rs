#![allow(dead_code)]

use parse_framework::{Parse, TokenStream};

use crate::*;

#[derive(Debug)]
pub enum Decl<'a> {
	Class(ClassDecl<'a>),
	Fun(FunDecl<'a>),
	Variable(VariableDecl<'a>),
}

#[derive(Debug)]
pub struct ClassDecl<'a> {
	pub name: Token<'a>,
	pub methods: Vec<FunDecl<'a>>,
	pub superclass: Option<Token<'a>>,
}

#[derive(Debug)]
pub struct FunDecl<'a> {
	pub name: Token<'a>,
	pub func: FunExpr<'a>,
}

#[derive(Debug)]
pub struct VariableDecl<'a> {
	pub name: Token<'a>,
	pub initializer: Option<Expr<'a>>,
}

impl<'a> Parse for Decl<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		use Token::*;

		match input.next() {
			Some(Keyword("class", _)) => Ok(Decl::Class(input.parse::<ClassDecl>()?)),
			Some(Keyword("fun", _)) => Ok(Decl::Fun(input.parse::<FunDecl>()?)),
			Some(Keyword("var", _)) => Ok(Decl::Variable(input.parse::<VariableDecl>()?)),
			Some(other) => Err(format!(
				"Expected `class`, `fun`, or `var`, but found `{}`",
				other
			)),
			None => Err("Unexpected end of input".into()),
		}
	}
}

impl<'a> Parse for ClassDecl<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		let name = input.take_kind(TokenKind::Ident)?;
		let superclass = if input.check(operator![<]) {
			input.take(operator![<])?;
			Some(input.take_kind(TokenKind::Ident)?)
		} else {
			None
		};

		input.take(brace!["{"])?;

		let mut methods = vec![];
		while !input.is_empty() && !input.check(brace!["}"]) {
			methods.push(input.parse::<FunDecl>()?);
		}

		input.take(brace!["}"])?;

		Ok(ClassDecl {
			name,
			superclass,
			methods,
		})
	}
}

impl<'a> Parse for FunDecl<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		let name = input.take_kind(TokenKind::Ident)?;
		let func = input.parse::<FunExpr>()?;

		Ok(FunDecl { name, func })
	}
}

impl<'a> Parse for VariableDecl<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		let name = input.take_kind(TokenKind::Ident)?;
		let initializer = if input.check(operator![=]) {
			input.take(operator![=])?;

			Some(input.parse::<Expr>()?)
		} else {
			None
		};

		input.take(punct![;])?;

		Ok(VariableDecl { name, initializer })
	}
}
