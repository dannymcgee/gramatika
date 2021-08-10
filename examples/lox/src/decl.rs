use gramatika::{Parse, ParseStreamer, Result, Spanned, SpannedError};

use crate::*;

#[derive(DebugLisp)]
pub enum Decl<'a> {
	Class(ClassDecl<'a>),
	Fun(FunDecl<'a>),
	Variable(VariableDecl<'a>),
}

#[derive(DebugLisp)]
pub struct ClassDecl<'a> {
	pub name: Token<'a>,
	pub superclass: Option<Token<'a>>,
	pub methods: Vec<FunDecl<'a>>,
}

#[derive(DebugLisp)]
pub struct FunDecl<'a> {
	pub name: Token<'a>,
	pub func: FunExpr<'a>,
}

#[derive(DebugLisp)]
pub struct VariableDecl<'a> {
	pub name: Token<'a>,
	pub initializer: Option<Expr<'a>>,
}

impl<'a> Parse<'a> for Decl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		use Token::*;

		match input.next() {
			Some(Keyword("class", _)) => Ok(Decl::Class(input.parse::<ClassDecl>()?)),
			Some(Keyword("fun", _)) => Ok(Decl::Fun(input.parse::<FunDecl>()?)),
			Some(Keyword("var", _)) => Ok(Decl::Variable(input.parse::<VariableDecl>()?)),
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

impl<'a> Parse<'a> for ClassDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
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

impl<'a> Parse<'a> for FunDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let name = input.consume_kind(TokenKind::Ident)?;
		let func = input.parse::<FunExpr>()?;

		Ok(FunDecl { name, func })
	}
}

impl<'a> Parse<'a> for VariableDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
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
