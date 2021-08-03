#![allow(dead_code)]

use parse_framework::{Parse, TokenStream};

use crate::*;

#[derive(Debug)]
pub struct Program<'a> {
	pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
	Block(Vec<Stmt<'a>>),
	Decl(Decl<'a>),
	Expr(Expr<'a>),
	For(ForStmt<'a>),
	If(IfStmt<'a>),
	Print(PrintStmt<'a>),
	Return(ReturnStmt<'a>),
	While(WhileStmt<'a>),
}

#[derive(Debug)]
pub struct ForStmt<'a> {
	pub keyword: Token<'a>,
	pub initializer: Option<Box<Stmt<'a>>>,
	pub condition: Option<Expr<'a>>,
	pub increment: Option<Expr<'a>>,
	pub body: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub struct IfStmt<'a> {
	pub keyword: Token<'a>,
	pub condition: Expr<'a>,
	pub then_branch: Box<Stmt<'a>>,
	pub else_branch: Option<Box<Stmt<'a>>>,
}

#[derive(Debug)]
pub struct PrintStmt<'a> {
	pub keyword: Token<'a>,
	pub value: Expr<'a>,
}

#[derive(Debug)]
pub struct ReturnStmt<'a> {
	pub keyword: Token<'a>,
	pub value: Option<Expr<'a>>,
}

#[derive(Debug)]
pub struct WhileStmt<'a> {
	pub keyword: Token<'a>,
	pub condition: Expr<'a>,
	pub body: Box<Stmt<'a>>,
}

impl<'a> Parse for Program<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		let mut stmts = vec![];
		while !input.is_empty() {
			stmts.push(input.parse::<Stmt>()?);
		}

		Ok(Self { stmts })
	}
}

impl<'a> Parse for Stmt<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		use Token::*;

		match input.peek() {
			Some(token) => match token {
				Keyword("class" | "fun" | "var", _) => {
					Ok(Stmt::Decl(input.parse::<Decl>()?))
				}
				Keyword("for", _) => Ok(Stmt::For(input.parse::<ForStmt>()?)),
				Keyword("if", _) => Ok(Stmt::If(input.parse::<IfStmt>()?)),
				Keyword("print", _) => Ok(Stmt::Print(input.parse::<PrintStmt>()?)),
				Keyword("return", _) => Ok(Stmt::Return(input.parse::<ReturnStmt>()?)),
				Keyword("while", _) => Ok(Stmt::While(input.parse::<WhileStmt>()?)),
				Brace("{", _) => {
					input.take(brace!["{"])?;

					let mut stmts = vec![];
					while !input.is_empty() && !input.check(brace!["}"]) {
						stmts.push(input.parse::<Stmt>()?);
					}

					input.take(brace!["}"])?;

					Ok(Stmt::Block(stmts))
				}
				_ => {
					let expr = input.parse::<Expr>()?;
					input.take(punct![;])?;

					Ok(Stmt::Expr(expr))
				}
			},
			None => Err("Unexpected end of input".into()),
		}
	}
}

impl<'a> Parse for ForStmt<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		let keyword = input.take(keyword![for])?;

		input.take(brace!["("])?;

		let initializer = if input.check(punct![;]) {
			input.take(punct![;])?;
			None
		} else {
			Some(Box::new(input.parse::<Stmt>()?))
		};

		let condition = if input.check(punct![;]) {
			None
		} else {
			Some(input.parse::<Expr>()?)
		};
		input.take(punct![;])?;

		let increment = if input.check(brace![")"]) {
			None
		} else {
			Some(input.parse::<Expr>()?)
		};

		input.take(brace![")"])?;
		input.take(brace!["{"])?;

		let mut body = vec![];
		while !input.is_empty() && !input.check(brace!["}"]) {
			body.push(input.parse::<Stmt>()?);
		}

		input.take(brace!["}"])?;

		Ok(ForStmt {
			keyword,
			initializer,
			condition,
			increment,
			body,
		})
	}
}

impl<'a> Parse for IfStmt<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		let keyword = input.take(keyword![if])?;

		input.take(brace!["("])?;
		let condition = input.parse::<Expr>()?;
		input.take(brace![")"])?;

		let then_branch = Box::new(input.parse::<Stmt>()?);
		let else_branch = if input.check(keyword![else]) {
			input.take(keyword![else])?;

			Some(Box::new(input.parse::<Stmt>()?))
		} else {
			None
		};

		Ok(IfStmt {
			keyword,
			condition,
			then_branch,
			else_branch,
		})
	}
}

impl<'a> Parse for PrintStmt<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		let keyword = input.take(keyword![print])?;
		let value = input.parse::<Expr>()?;

		input.take(punct![;])?;

		Ok(PrintStmt { keyword, value })
	}
}

impl<'a> Parse for ReturnStmt<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		let keyword = input.take(keyword![return])?;
		let value = if input.check(punct![;]) {
			None
		} else {
			Some(input.parse::<Expr>()?)
		};

		input.take(punct![;])?;

		Ok(ReturnStmt { keyword, value })
	}
}

impl<'a> Parse for WhileStmt<'a> {
	type Token = crate::Token<'a>;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized {
		let keyword = input.take(keyword![while])?;

		input.take(brace!["("])?;
		let condition = input.parse::<Expr>()?;
		input.take(brace![")"])?;

		let body = Box::new(input.parse::<Stmt>()?);

		Ok(WhileStmt {
			keyword,
			condition,
			body,
		})
	}
}
