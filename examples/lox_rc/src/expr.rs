use gramatika::{Parse, ParseStreamer, Result, Spanned, SpannedError};

use crate::{
	brace, keyword, operator,
	parse::ParseStream,
	punct,
	stmt::Stmt,
	tokens::{Keyword, Token, TokenKind},
};

#[derive(DebugLisp)]
pub enum Expr {
	Assignment(AssignmentExpr),
	Binary(BinaryExpr),
	Fun(FunExpr),
	FunCall(FunCallExpr),
	Get(GetExpr),
	Grouping(Box<Expr>),
	Literal(Token),
	Logical(BinaryExpr),
	Set(SetExpr),
	Super(SuperExpr),
	This(Token),
	Unary(UnaryExpr),
	Variable(Token),
}

#[derive(DebugLisp)]
pub struct AssignmentExpr {
	pub name: Token,
	pub value: Box<Expr>,
}

#[derive(DebugLisp)]
pub struct BinaryExpr {
	pub lhs: Box<Expr>,
	pub op: Token,
	pub rhs: Box<Expr>,
}

#[derive(DebugLisp)]
pub struct FunExpr {
	pub params: Vec<Token>,
	pub body: Vec<Stmt>,
}

#[derive(DebugLisp)]
pub struct FunCallExpr {
	pub callee: Box<Expr>,
	pub args: Vec<Expr>,
}

#[derive(DebugLisp)]
pub struct GetExpr {
	pub obj: Box<Expr>,
	pub name: Token,
}

#[derive(DebugLisp)]
pub struct SetExpr {
	pub obj: Box<Expr>,
	pub name: Token,
	pub value: Box<Expr>,
}

#[derive(DebugLisp)]
pub struct SuperExpr {
	pub keyword: Token,
	pub method: Token,
}

#[derive(DebugLisp)]
pub struct UnaryExpr {
	pub op: Token,
	pub rhs: Box<Expr>,
}

trait RecursiveDescent {
	type Token: gramatika::Token;

	fn assignment(&mut self) -> Result<Expr>;
	fn or(&mut self) -> Result<Expr>;
	fn and(&mut self) -> Result<Expr>;
	fn equality(&mut self) -> Result<Expr>;
	fn comparison(&mut self) -> Result<Expr>;
	fn term(&mut self) -> Result<Expr>;
	fn factor(&mut self) -> Result<Expr>;
	fn unary(&mut self) -> Result<Expr>;
	fn call(&mut self) -> Result<Expr>;
	fn finish_call(&mut self, callee: Expr) -> Result<Expr>;
	fn primary(&mut self) -> Result<Expr>;
	fn binary(
		&mut self,
		operators: &[Self::Token],
		operand_method: fn(&mut Self) -> Result<Expr>,
	) -> Result<Expr>;
}

impl Parse for Expr {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		input.assignment()
	}
}

impl Parse for FunExpr {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
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

impl RecursiveDescent for ParseStream {
	type Token = Token;

	fn assignment(&mut self) -> Result<Expr> {
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

	fn or(&mut self) -> Result<Expr> {
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

	fn and(&mut self) -> Result<Expr> {
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

	fn equality(&mut self) -> Result<Expr> {
		self.binary(&[operator![==], operator![!=]], Self::comparison)
	}

	fn comparison(&mut self) -> Result<Expr> {
		self.binary(
			&[operator![>], operator![>=], operator![<], operator![<=]],
			Self::term,
		)
	}

	fn term(&mut self) -> Result<Expr> {
		self.binary(&[operator!["-"], operator![+]], Self::factor)
	}

	fn factor(&mut self) -> Result<Expr> {
		self.binary(&[operator![/], operator![*]], Self::unary)
	}

	fn unary(&mut self) -> Result<Expr> {
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

	fn call(&mut self) -> Result<Expr> {
		use Token::*;

		let mut expr = self.primary()?;
		expr = loop {
			match self.peek() {
				Some(Brace(lex, _)) if lex == "(" => {
					self.consume(brace!["("])?;

					expr = self.finish_call(expr)?;
				}
				Some(Punct(lex, _)) if lex == "." => {
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

	fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
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

	fn primary(&mut self) -> Result<Expr> {
		match self.next() {
			Some(token) => match token.clone() {
				Token::NumLit(_, _) | Token::StrLit(_, _) => Ok(Expr::Literal(token)),
				Token::Keyword(Keyword::True | Keyword::False | Keyword::Nil, _) => {
					Ok(Expr::Literal(token))
				}
				Token::Keyword(Keyword::Super, _) => {
					self.consume(punct![.])?;
					let method = self.consume_kind(TokenKind::Ident)?;

					Ok(Expr::Super(SuperExpr {
						keyword: token,
						method,
					}))
				}
				Token::Keyword(Keyword::This, _) => Ok(Expr::This(token)),
				Token::Keyword(Keyword::Fun, _) => Ok(Expr::Fun(self.parse()?)),
				Token::Brace(lex, _) if lex == "(" => {
					let expr = self.parse::<Expr>()?;
					self.consume(brace![")"])?;

					Ok(Expr::Grouping(Box::new(expr)))
				}
				Token::Ident(_, _) => Ok(Expr::Variable(token)),
				other => Err(SpannedError {
					message: "Expected expression".into(),
					source: self.source(),
					span: Some(other.span()),
				}),
			},
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
		operand_method: fn(&mut Self) -> Result<Expr>,
	) -> Result<Expr> {
		let mut expr = operand_method(self)?;

		while operators.iter().any(|op| self.check(op.clone())) {
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
