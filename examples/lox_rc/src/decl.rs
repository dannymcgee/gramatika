use gramatika::{Parse, ParseStreamer, Result, Spanned, SpannedError};

use crate::{
	brace,
	expr::{Expr, FunExpr},
	operator,
	parse::ParseStream,
	punct,
	tokens::{Token, TokenKind},
};

#[derive(DebugLisp)]
pub enum Decl {
	Class(ClassDecl),
	Fun(FunDecl),
	Variable(VariableDecl),
}

#[derive(DebugLisp)]
pub struct ClassDecl {
	pub name: Token,
	pub superclass: Option<Token>,
	pub methods: Vec<FunDecl>,
}

#[derive(DebugLisp)]
pub struct FunDecl {
	pub name: Token,
	pub func: FunExpr,
}

#[derive(DebugLisp)]
pub struct VariableDecl {
	pub name: Token,
	pub initializer: Option<Expr>,
}

impl Parse for Decl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		use Token::*;

		match input.next() {
			Some(Keyword(lex, _)) if lex == "class" => {
				Ok(Decl::Class(input.parse::<ClassDecl>()?))
			}
			Some(Keyword(lex, _)) if lex == "fun" => {
				Ok(Decl::Fun(input.parse::<FunDecl>()?))
			}
			Some(Keyword(lex, _)) if lex == "var" => {
				Ok(Decl::Variable(input.parse::<VariableDecl>()?))
			}
			Some(other) => Err(SpannedError {
				message: "Expected `class`, `fun`, or `var`".into(),
				source: input.source(),
				span: Some(other.span()),
			}),
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: None,
			}),
		}
	}
}

impl Parse for ClassDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let name = input.consume_kind(TokenKind::Ident)?;
		let superclass = if input.check(operator![<]) {
			input.consume(operator![<])?;
			Some(input.consume_kind(TokenKind::Ident)?)
		} else {
			None
		};

		input.consume(brace!["{"])?;

		let mut methods = vec![];
		while !input.is_empty() && !input.check(brace!["}"]) {
			methods.push(input.parse::<FunDecl>()?);
		}

		input.consume(brace!["}"])?;

		Ok(ClassDecl {
			name,
			superclass,
			methods,
		})
	}
}

impl Parse for FunDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let name = input.consume_kind(TokenKind::Ident)?;
		let func = input.parse::<FunExpr>()?;

		Ok(FunDecl { name, func })
	}
}

impl Parse for VariableDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let name = input.consume_kind(TokenKind::Ident)?;
		let initializer = if input.check(operator![=]) {
			input.consume(operator![=])?;

			Some(input.parse::<Expr>()?)
		} else {
			None
		};

		input.consume(punct![;])?;

		Ok(VariableDecl { name, initializer })
	}
}
