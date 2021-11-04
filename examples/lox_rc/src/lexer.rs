use arcstr::{ArcStr, Substr};
use gramatika::{Position, Span};

use crate::tokens::Token;

pub struct Lexer {
	input: ArcStr,
	remaining: Substr,
	current: Position,
	lookahead: Position,
}

impl Lexer {
	pub fn new(input: ArcStr) -> Self {
		Self {
			remaining: input.substr(..),
			input,
			current: Position::default(),
			lookahead: Position::default(),
		}
	}
}

type TokenCtor = fn(lexeme: Substr, span: Span) -> Token;

impl gramatika::Lexer for Lexer {
	type Output = Token;

	fn new(input: ArcStr) -> Self {
		Self {
			remaining: input.substr(..),
			input,
			current: Position::default(),
			lookahead: Position::default(),
		}
	}

	fn source(&self) -> ArcStr {
		self.input.clone()
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
		let result = None
			.or_else(|| Token::match_keyword(&self.remaining).map(|m| (m, Token::keyword as TokenCtor)))
			.or_else(|| Token::match_ident(&self.remaining).map(|m| (m, Token::ident as TokenCtor)))
			.or_else(|| Token::match_brace(&self.remaining).map(|m| (m, Token::brace as TokenCtor)))
			.or_else(|| Token::match_punct(&self.remaining).map(|m| (m, Token::punct as TokenCtor)))
			.or_else(|| Token::match_operator(&self.remaining).map(|m| (m, Token::operator as TokenCtor)))
			.or_else(|| Token::match_num_lit(&self.remaining).map(|m| (m, Token::num_lit as TokenCtor)))
			.or_else(|| Token::match_str_lit(&self.remaining).map(|m| (m, Token::str_lit as TokenCtor)));

		match result {
			Some((m, ctor)) => {
				let match_end = m.end();
				let lexeme = self.remaining.substr_from(m.as_str());

				self.lookahead.character += match_end;

				let span = Span {
					start: self.current,
					end: self.lookahead,
				};
				let token = ctor(lexeme, span);

				self.remaining = self.remaining.substr(match_end..);
				self.current = self.lookahead;

				Some(token)
			}
			None => {
				self.remaining.clone().chars().next().and_then(|c| match c {
					' ' | '\t' | '\r' => {
						self.lookahead.character += 1;
						self.current.character += 1;
						self.remaining = self.remaining.substr(1..);

						self.scan_token()
					}
					'\n' => {
						self.lookahead.line += 1;
						self.lookahead.character = 0;
						self.current = self.lookahead;
						self.remaining = self.remaining.substr(1..);

						self.scan_token()
					}
					other => panic!("Unsupported input: `{}` at {:?}", other, self.current),
				})
			}
		}
	}
}
