#![allow(dead_code)]

use parse_framework::{Parse, ParseStreamer, Token as _};

use crate::*;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct AssignmentExpr<'a> {
	pub name: Token<'a>,
	pub value: Box<Expr<'a>>,
}

#[derive(Debug)]
pub struct BinaryExpr<'a> {
	pub lhs: Box<Expr<'a>>,
	pub op: Token<'a>,
	pub rhs: Box<Expr<'a>>,
}

#[derive(Debug)]
pub struct FunExpr<'a> {
	pub params: Vec<Token<'a>>,
	pub body: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub struct FunCallExpr<'a> {
	pub callee: Box<Expr<'a>>,
	pub args: Vec<Expr<'a>>,
}

#[derive(Debug)]
pub struct GetExpr<'a> {
	pub obj: Box<Expr<'a>>,
	pub name: Token<'a>,
}

#[derive(Debug)]
pub struct SetExpr<'a> {
	pub obj: Box<Expr<'a>>,
	pub name: Token<'a>,
	pub value: Box<Expr<'a>>,
}

#[derive(Debug)]
pub struct SuperExpr<'a> {
	pub keyword: Token<'a>,
	pub method: Token<'a>,
}

#[derive(Debug)]
pub struct UnaryExpr<'a> {
	pub op: Token<'a>,
	pub rhs: Box<Expr<'a>>,
}

trait RecursiveDescent<'a> {
	type Token: parse_framework::Token;

	fn assignment(&mut self) -> Result<Expr<'a>, String>;
	fn or(&mut self) -> Result<Expr<'a>, String>;
	fn and(&mut self) -> Result<Expr<'a>, String>;
	fn equality(&mut self) -> Result<Expr<'a>, String>;
	fn comparison(&mut self) -> Result<Expr<'a>, String>;
	fn term(&mut self) -> Result<Expr<'a>, String>;
	fn factor(&mut self) -> Result<Expr<'a>, String>;
	fn unary(&mut self) -> Result<Expr<'a>, String>;
	fn call(&mut self) -> Result<Expr<'a>, String>;
	fn finish_call(&mut self, callee: Expr<'a>) -> Result<Expr<'a>, String>;
	fn primary(&mut self) -> Result<Expr<'a>, String>;
	fn binary(
		&mut self,
		operators: &[Self::Token],
		operand_method: fn(&mut Self) -> Result<Expr<'a>, String>,
	) -> Result<Expr<'a>, String>;
}

impl<'a> Parse for Expr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<Self, String>
	where Self: Sized {
		input.assignment()
	}
}

impl<'a> Parse for FunExpr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<Self, String>
	where Self: Sized {
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

	fn assignment(&mut self) -> Result<Expr<'a>, String> {
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
				other => Err(format!("Invalid assignment target: {:?}", other)),
			}
		} else {
			Ok(expr)
		}
	}

	fn or(&mut self) -> Result<Expr<'a>, String> {
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

	fn and(&mut self) -> Result<Expr<'a>, String> {
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

	fn equality(&mut self) -> Result<Expr<'a>, String> {
		self.binary(&[operator![==], operator![!=]], Self::comparison)
	}

	fn comparison(&mut self) -> Result<Expr<'a>, String> {
		self.binary(
			&[operator![>], operator![>=], operator![<], operator![<=]],
			Self::term,
		)
	}

	fn term(&mut self) -> Result<Expr<'a>, String> {
		self.binary(&[operator!["-"], operator![+]], Self::factor)
	}

	fn factor(&mut self) -> Result<Expr<'a>, String> {
		self.binary(&[operator![/], operator![*]], Self::unary)
	}

	fn unary(&mut self) -> Result<Expr<'a>, String> {
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

	fn call(&mut self) -> Result<Expr<'a>, String> {
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

	fn finish_call(&mut self, callee: Expr<'a>) -> Result<Expr<'a>, String> {
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

	fn primary(&mut self) -> Result<Expr<'a>, String> {
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
			Some(other) => Err(format!("Expected expression, found {:?}", other)),
			None => Err("Unexpected end of input".into()),
		}
	}

	fn binary(
		&mut self,
		operators: &[Self::Token],
		operand_method: fn(&mut Self) -> Result<Expr<'a>, String>,
	) -> Result<Expr<'a>, String> {
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
