use arcstr::{ArcStr, Substr};
use gramatika::{Position, Span};

use crate::tokens::{Token, TokenKind};

pub struct Lexer {
	input: ArcStr,
	remaining: Substr,
	current: Position,
	lookahead: Position,
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
			.or_else(|| Token::match_comment(&self.remaining))
			.or_else(|| Token::match_ident(&self.remaining))
			.or_else(|| Token::match_brace(&self.remaining))
			.or_else(|| Token::match_punct(&self.remaining))
			.or_else(|| Token::match_operator(&self.remaining))
			.or_else(|| Token::match_num_lit(&self.remaining))
			.or_else(|| Token::match_str_lit(&self.remaining));

		match result {
			Some((start, end, kind)) => {
				let ctor = match kind {
					TokenKind::Comment => Token::comment as TokenCtor,
					TokenKind::Keyword => Token::keyword as TokenCtor,
					TokenKind::Ident => Token::ident as TokenCtor,
					TokenKind::Brace => Token::brace as TokenCtor,
					TokenKind::Punct => Token::punct as TokenCtor,
					TokenKind::Operator => Token::operator as TokenCtor,
					TokenKind::NumLit => Token::num_lit as TokenCtor,
					TokenKind::StrLit => Token::str_lit as TokenCtor,
				};
				let lexeme = self.remaining.substr(start..end);

				self.lookahead.character += end;

				let span = Span {
					start: self.current,
					end: self.lookahead,
				};
				let token = ctor(lexeme, span);

				self.remaining = self.remaining.substr(end..);
				self.current = self.lookahead;

				if TokenKind::discards().contains(&kind) {
					self.scan_token()
				} else {
					Some(token)
				}
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
