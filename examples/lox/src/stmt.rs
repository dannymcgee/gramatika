use gramatika::{Parse, ParseStreamer, Result, SpannedError};

use crate::*;

#[derive(DebugLisp)]
pub struct Program<'a> {
	pub stmts: Vec<Stmt<'a>>,
}

#[derive(DebugLisp)]
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

#[derive(DebugLisp)]
pub struct ForStmt<'a> {
	pub keyword: Token<'a>,
	pub initializer: Option<Box<Stmt<'a>>>,
	pub condition: Option<Expr<'a>>,
	pub increment: Option<Expr<'a>>,
	pub body: Box<Stmt<'a>>,
}

#[derive(DebugLisp)]
pub struct IfStmt<'a> {
	pub keyword: Token<'a>,
	pub condition: Expr<'a>,
	pub then_branch: Box<Stmt<'a>>,
	pub else_branch: Option<Box<Stmt<'a>>>,
}

#[derive(DebugLisp)]
pub struct PrintStmt<'a> {
	pub keyword: Token<'a>,
	pub value: Expr<'a>,
}

#[derive(DebugLisp)]
pub struct ReturnStmt<'a> {
	pub keyword: Token<'a>,
	pub value: Option<Expr<'a>>,
}

#[derive(DebugLisp)]
pub struct WhileStmt<'a> {
	pub keyword: Token<'a>,
	pub condition: Expr<'a>,
	pub body: Box<Stmt<'a>>,
}

impl<'a> Parse<'a> for Program<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let mut stmts = vec![];
		while !input.is_empty() {
			stmts.push(input.parse::<Stmt>()?);
		}

		Ok(Self { stmts })
	}
}

impl<'a> Parse<'a> for Stmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
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
					input.consume(brace!["{"])?;

					let mut stmts = vec![];
					while !input.is_empty() && !input.check(brace!["}"]) {
						stmts.push(input.parse::<Stmt>()?);
					}

					input.consume(brace!["}"])?;

					Ok(Stmt::Block(stmts))
				}
				_ => {
					let expr = input.parse::<Expr>()?;
					input.consume(punct![;])?;

					Ok(Stmt::Expr(expr))
				}
			},
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: None,
			}),
		}
	}
}

impl<'a> Parse<'a> for ForStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let keyword = input.consume(keyword![for])?;

		input.consume(brace!["("])?;

		let initializer = if input.check(punct![;]) {
			input.consume(punct![;])?;
			None
		} else {
			Some(Box::new(input.parse::<Stmt>()?))
		};

		let condition = if input.check(punct![;]) {
			None
		} else {
			Some(input.parse::<Expr>()?)
		};
		input.consume(punct![;])?;

		let increment = if input.check(brace![")"]) {
			None
		} else {
			Some(input.parse::<Expr>()?)
		};

		input.consume(brace![")"])?;

		let body = Box::new(input.parse::<Stmt>()?);

		Ok(ForStmt {
			keyword,
			initializer,
			condition,
			increment,
			body,
		})
	}
}

impl<'a> Parse<'a> for IfStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let keyword = input.consume(keyword![if])?;

		input.consume(brace!["("])?;
		let condition = input.parse::<Expr>()?;
		input.consume(brace![")"])?;

		let then_branch = Box::new(input.parse::<Stmt>()?);
		let else_branch = if input.check(keyword![else]) {
			input.consume(keyword![else])?;

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

impl<'a> Parse<'a> for PrintStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let keyword = input.consume(keyword![print])?;
		let value = input.parse::<Expr>()?;

		input.consume(punct![;])?;

		Ok(PrintStmt { keyword, value })
	}
}

impl<'a> Parse<'a> for ReturnStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let keyword = input.consume(keyword![return])?;
		let value = if input.check(punct![;]) {
			None
		} else {
			Some(input.parse::<Expr>()?)
		};

		input.consume(punct![;])?;

		Ok(ReturnStmt { keyword, value })
	}
}

impl<'a> Parse<'a> for WhileStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let keyword = input.consume(keyword![while])?;

		input.consume(brace!["("])?;
		let condition = input.parse::<Expr>()?;
		input.consume(brace![")"])?;

		let body = Box::new(input.parse::<Stmt>()?);

		Ok(WhileStmt {
			keyword,
			condition,
			body,
		})
	}
}
