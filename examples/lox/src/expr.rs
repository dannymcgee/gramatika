use gramatika::{Parse, ParseStreamer, Result, Spanned, SpannedError};

use crate::*;

#[derive(DebugLisp)]
pub enum Expr<'a> {
	Assignment(AssignmentExpr<'a>),
	Binary(BinaryExpr<'a>),
	Fun(FunExpr<'a>),
	FunCall(FunCallExpr<'a>),
	Get(GetExpr<'a>),
	Grouping(Box<Expr<'a>>),
	Literal(Token<'a>),
	Logical(BinaryExpr<'a>),
	Set(SetExpr<'a>),
	Super(SuperExpr<'a>),
	This(Token<'a>),
	Unary(UnaryExpr<'a>),
	Variable(Token<'a>),
}

#[derive(DebugLisp)]
pub struct AssignmentExpr<'a> {
	pub name: Token<'a>,
	pub value: Box<Expr<'a>>,
}

#[derive(DebugLisp)]
pub struct BinaryExpr<'a> {
	pub lhs: Box<Expr<'a>>,
	pub op: Token<'a>,
	pub rhs: Box<Expr<'a>>,
}

#[derive(DebugLisp)]
pub struct FunExpr<'a> {
	pub params: Vec<Token<'a>>,
	pub body: Vec<Stmt<'a>>,
}

#[derive(DebugLisp)]
pub struct FunCallExpr<'a> {
	pub callee: Box<Expr<'a>>,
	pub args: Vec<Expr<'a>>,
}

#[derive(DebugLisp)]
pub struct GetExpr<'a> {
	pub obj: Box<Expr<'a>>,
	pub name: Token<'a>,
}

#[derive(DebugLisp)]
pub struct SetExpr<'a> {
	pub obj: Box<Expr<'a>>,
	pub name: Token<'a>,
	pub value: Box<Expr<'a>>,
}

#[derive(DebugLisp)]
pub struct SuperExpr<'a> {
	pub keyword: Token<'a>,
	pub method: Token<'a>,
}

#[derive(DebugLisp)]
pub struct UnaryExpr<'a> {
	pub op: Token<'a>,
	pub rhs: Box<Expr<'a>>,
}

trait RecursiveDescent<'a> {
	type Token: gramatika::Token;

	fn assignment(&mut self) -> Result<'a, Expr<'a>>;
	fn or(&mut self) -> Result<'a, Expr<'a>>;
	fn and(&mut self) -> Result<'a, Expr<'a>>;
	fn equality(&mut self) -> Result<'a, Expr<'a>>;
	fn comparison(&mut self) -> Result<'a, Expr<'a>>;
	fn term(&mut self) -> Result<'a, Expr<'a>>;
	fn factor(&mut self) -> Result<'a, Expr<'a>>;
	fn unary(&mut self) -> Result<'a, Expr<'a>>;
	fn call(&mut self) -> Result<'a, Expr<'a>>;
	fn finish_call(&mut self, callee: Expr<'a>) -> Result<'a, Expr<'a>>;
	fn primary(&mut self) -> Result<'a, Expr<'a>>;
	fn binary(
		&mut self,
		operators: &[Self::Token],
		operand_method: fn(&mut Self) -> Result<'a, Expr<'a>>,
	) -> Result<'a, Expr<'a>>;
}

impl<'a> Parse<'a> for Expr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		input.assignment()
	}
}

impl<'a> Parse<'a> for FunExpr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		input.consume(brace!["("])?;

		let mut params = vec![];
		while !input.is_empty() && !input.check(brace![")"]) {
			if !params.is_empty() {
				input.consume(punct![,])?;
			}
			params.push(input.consume_kind(TokenKind::Ident)?);
		}

		input.consume(brace![")"])?;
		input.consume(brace!["{"])?;

		let mut body = vec![];
		while !input.is_empty() && !input.check(brace!["}"]) {
			body.push(input.parse::<Stmt>()?);
		}

		input.consume(brace!["}"])?;

		Ok(FunExpr { params, body })
	}
}

impl<'a> RecursiveDescent<'a> for ParseStream<'a> {
	type Token = Token<'a>;

	fn assignment(&mut self) -> Result<'a, Expr<'a>> {
		let expr = self.or()?;

		if self.check(operator![=]) {
			self.consume(operator![=])?;

			let value = self.assignment()?;

			match expr {
				Expr::Variable(name) => Ok(Expr::Assignment(AssignmentExpr {
					name,
					value: Box::new(value),
				})),
				Expr::Get(expr) => Ok(Expr::Set(SetExpr {
					obj: expr.obj,
					name: expr.name,
					value: Box::new(value),
				})),
				_other => Err(SpannedError {
					message: "Invalid assignment target".into(),
					source: self.source(),
					span: None, // FIXME
				}),
			}
		} else {
			Ok(expr)
		}
	}

	fn or(&mut self) -> Result<'a, Expr<'a>> {
		let mut expr = self.and()?;

		while self.check(keyword![or]) {
			let op = self.consume(keyword![or])?;
			let rhs = self.and()?;

			expr = Expr::Logical(BinaryExpr {
				lhs: Box::new(expr),
				op,
				rhs: Box::new(rhs),
			});
		}

		Ok(expr)
	}

	fn and(&mut self) -> Result<'a, Expr<'a>> {
		let mut expr = self.equality()?;

		while self.check(keyword![and]) {
			let op = self.consume(keyword![and])?;
			let rhs = self.equality()?;

			expr = Expr::Logical(BinaryExpr {
				lhs: Box::new(expr),
				op,
				rhs: Box::new(rhs),
			});
		}

		Ok(expr)
	}

	fn equality(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![==], operator![!=]], Self::comparison)
	}

	fn comparison(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(
			&[operator![>], operator![>=], operator![<], operator![<=]],
			Self::term,
		)
	}

	fn term(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator!["-"], operator![+]], Self::factor)
	}

	fn factor(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![/], operator![*]], Self::unary)
	}

	fn unary(&mut self) -> Result<'a, Expr<'a>> {
		if self.check(operator![!]) || self.check(operator!["-"]) {
			let op = self.consume_kind(TokenKind::Operator)?;
			let rhs = self.unary()?;

			Ok(Expr::Unary(UnaryExpr {
				op,
				rhs: Box::new(rhs),
			}))
		} else {
			self.call()
		}
	}

	fn call(&mut self) -> Result<'a, Expr<'a>> {
		use Token::*;

		let mut expr = self.primary()?;
		expr = loop {
			match self.peek() {
				Some(Brace("(", _)) => {
					self.consume(brace!["("])?;

					expr = self.finish_call(expr)?;
				}
				Some(Punct(".", _)) => {
					self.consume(punct![.])?;
					let name = self.consume_kind(TokenKind::Ident)?;

					expr = Expr::Get(GetExpr {
						obj: Box::new(expr),
						name,
					});
				}
				Some(_) | None => break expr,
			}
		};

		Ok(expr)
	}

	fn finish_call(&mut self, callee: Expr<'a>) -> Result<'a, Expr<'a>> {
		let mut args = vec![];
		while !self.is_empty() && !self.check(brace![")"]) {
			if !args.is_empty() {
				self.consume(punct![,])?;
			}
			args.push(self.parse::<Expr>()?);
		}

		self.consume(brace![")"])?;

		Ok(Expr::FunCall(FunCallExpr {
			callee: Box::new(callee),
			args,
		}))
	}

	fn primary(&mut self) -> Result<'a, Expr<'a>> {
		use Token::*;

		match self.next() {
			Some(
				token @ Keyword("true" | "false" | "nil", _)
				| token @ NumLit(_, _)
				| token @ StrLit(_, _),
			) => Ok(Expr::Literal(token)),
			Some(keyword @ Keyword("super", _)) => {
				self.consume(punct![.])?;
				let method = self.consume_kind(TokenKind::Ident)?;

				Ok(Expr::Super(SuperExpr { keyword, method }))
			}
			Some(token @ Keyword("this", _)) => Ok(Expr::This(token)),
			Some(token @ Ident(_, _)) => Ok(Expr::Variable(token)),
			Some(Keyword("fun", _)) => Ok(Expr::Fun(self.parse::<FunExpr>()?)),
			Some(Brace("(", _)) => {
				let expr = self.parse::<Expr>()?;
				self.consume(brace![")"])?;

				Ok(Expr::Grouping(Box::new(expr)))
			}
			Some(other) => Err(SpannedError {
				message: "Expected expression".into(),
				source: self.source(),
				span: Some(other.span()),
			}),
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: self.source(),
				span: None,
			}),
		}
	}

	fn binary(
		&mut self,
		operators: &[Self::Token],
		operand_method: fn(&mut Self) -> Result<'a, Expr<'a>>,
	) -> Result<'a, Expr<'a>> {
		let mut expr = operand_method(self)?;

		while operators.iter().any(|op| self.check(*op)) {
			let op = self.consume_kind(TokenKind::Operator)?;
			let rhs = operand_method(self)?;

			expr = Expr::Binary(BinaryExpr {
				lhs: Box::new(expr),
				op,
				rhs: Box::new(rhs),
			});
		}

		Ok(expr)
	}
}
