use gramatika::{Parse, ParseStreamer, Result, SpannedError};

use crate::{
	brace,
	decl::Decl,
	expr::Expr,
	keyword,
	parse::ParseStream,
	punct,
	tokens::{Keyword, Token},
};

#[derive(DebugLisp)]
pub struct Program {
	pub stmts: Vec<Stmt>,
}

#[derive(DebugLisp)]
pub enum Stmt {
	Block(Vec<Stmt>),
	Decl(Decl),
	Expr(Expr),
	For(ForStmt),
	If(IfStmt),
	Print(PrintStmt),
	Return(ReturnStmt),
	While(WhileStmt),
}

#[derive(DebugLisp)]
pub struct ForStmt {
	pub keyword: Token,
	pub initializer: Option<Box<Stmt>>,
	pub condition: Option<Expr>,
	pub increment: Option<Expr>,
	pub body: Box<Stmt>,
}

#[derive(DebugLisp)]
pub struct IfStmt {
	pub keyword: Token,
	pub condition: Expr,
	pub then_branch: Box<Stmt>,
	pub else_branch: Option<Box<Stmt>>,
}

#[derive(DebugLisp)]
pub struct PrintStmt {
	pub keyword: Token,
	pub value: Expr,
}

#[derive(DebugLisp)]
pub struct ReturnStmt {
	pub keyword: Token,
	pub value: Option<Expr>,
}

#[derive(DebugLisp)]
pub struct WhileStmt {
	pub keyword: Token,
	pub condition: Expr,
	pub body: Box<Stmt>,
}

impl Parse for Program {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let mut stmts = vec![];
		while !input.is_empty() {
			stmts.push(input.parse::<Stmt>()?);
		}

		Ok(Self { stmts })
	}
}

impl Parse for Stmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		match input.peek() {
			Some(token) => match token {
				Token::Keyword(Keyword::Class | Keyword::Fun | Keyword::Var, _) => {
					Ok(Stmt::Decl(input.parse::<Decl>()?))
				}
				Token::Keyword(Keyword::For, _) => {
					Ok(Stmt::For(input.parse::<ForStmt>()?))
				}
				Token::Keyword(Keyword::If, _) => Ok(Stmt::If(input.parse::<IfStmt>()?)),
				Token::Keyword(Keyword::Print, _) => {
					Ok(Stmt::Print(input.parse::<PrintStmt>()?))
				}
				Token::Keyword(Keyword::Return, _) => {
					Ok(Stmt::Return(input.parse::<ReturnStmt>()?))
				}
				Token::Keyword(Keyword::While, _) => {
					Ok(Stmt::While(input.parse::<WhileStmt>()?))
				}
				Token::Brace(lex, _) if lex == "{" => {
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

impl Parse for ForStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
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

impl Parse for IfStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
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

impl Parse for PrintStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let keyword = input.consume(keyword![print])?;
		let value = input.parse::<Expr>()?;

		input.consume(punct![;])?;

		Ok(PrintStmt { keyword, value })
	}
}

impl Parse for ReturnStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
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

impl Parse for WhileStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
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
