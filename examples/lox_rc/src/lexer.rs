use std::rc::Rc;

use gramatika::{Position, Span};

use crate::tokens::Token;

pub struct Lexer {
	input: Rc<str>,
	remaining: Rc<str>,
	current: Position,
	lookahead: Position,
}

impl Lexer {
	pub fn new(input: Rc<str>) -> Self {
		Self {
			remaining: Rc::clone(&input),
			input,
			current: Position::default(),
			lookahead: Position::default(),
		}
	}
}

type TokenCtor = fn(lexeme: Rc<str>, span: Span) -> Token;

impl gramatika::Lexer for Lexer {
	type Input = Rc<str>;
	type Output = Token;

	fn new(input: Self::Input) -> Self {
		Self {
			remaining: Rc::clone(&input),
			input,
			current: Position::default(),
			lookahead: Position::default(),
		}
	}

	fn source(&self) -> Self::Input {
		Rc::clone(&self.input)
	}

	fn scan(&mut self) -> Vec<Self::Output> {
		let mut output = vec![];
		while let Some(token) = self.scan_token() {
			output.push(token);
		}

		output
	}

	#[rustfmt::skip]
	fn scan_token(&mut self) -> Option<Self::Output> {
		None
		.or_else(|| Token::match_keyword(Rc::clone(&self.remaining)).map(|m| (m, Token::keyword as TokenCtor)))
		.or_else(|| Token::match_ident(Rc::clone(&self.remaining)).map(|m| (m, Token::ident as TokenCtor)))
		.or_else(|| Token::match_brace(Rc::clone(&self.remaining)).map(|m| (m, Token::brace as TokenCtor)))
		.or_else(|| Token::match_punct(Rc::clone(&self.remaining)).map(|m| (m, Token::punct as TokenCtor)))
		.or_else(|| Token::match_operator(Rc::clone(&self.remaining)).map(|m| (m, Token::operator as TokenCtor)))
		.or_else(|| Token::match_num_lit(Rc::clone(&self.remaining)).map(|m| (m, Token::num_lit as TokenCtor)))
		.or_else(|| Token::match_str_lit(Rc::clone(&self.remaining)).map(|m| (m, Token::str_lit as TokenCtor)))
		.map(|(m, ctor)| {
			self.lookahead.character += m.end();

			let lexeme = m.as_str();
			let span = Span {
				start: self.current,
				end: self.lookahead,
			};
			let token = ctor(lexeme, span);

			self.remaining = (*self.remaining)[m.end()..].into();
			self.current = self.lookahead;

			token
		})
		.or_else(|| {
			Rc::clone(&self.remaining).chars().next().and_then(|c| match c {
				' ' | '\t' | '\r' => {
					self.lookahead.character += 1;
					self.current.character += 1;
					self.remaining = (*self.remaining)[1..].into();

					self.scan_token()
				}
				'\n' => {
					self.lookahead.line += 1;
					self.lookahead.character = 0;
					self.current = self.lookahead;
					self.remaining = (*self.remaining)[1..].into();

					self.scan_token()
				}
				other => panic!("Unsupported input: `{}` at {:?}", other, self.current),
			})
		})
	}
}
