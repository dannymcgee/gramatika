use arcstr::{ArcStr, Substr};
use gramatika::{Position, Span};

use crate::tokens::{Keyword, Token};

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
		let keyword = match &self.remaining {
			x if x.len() >= 3 && x[..3] == Keyword::AND => Some((Keyword::AND, Keyword::And)),
			x if x.len() >= 5 && x[..5] == Keyword::CLASS => Some((Keyword::CLASS, Keyword::Class)),
			x if x.len() >= 4 && x[..4] == Keyword::ELSE => Some((Keyword::ELSE, Keyword::Else)),
			x if x.len() >= 5 && x[..5] == Keyword::FALSE => Some((Keyword::FALSE, Keyword::False)),
			x if x.len() >= 3 && x[..3] == Keyword::FOR => Some((Keyword::FOR, Keyword::For)),
			x if x.len() >= 3 && x[..3] == Keyword::FUN => Some((Keyword::FUN, Keyword::Fun)),
			x if x.len() >= 2 && x[..2] == Keyword::IF => Some((Keyword::IF, Keyword::If)),
			x if x.len() >= 3 && x[..3] == Keyword::NIL => Some((Keyword::NIL, Keyword::Nil)),
			x if x.len() >= 2 && x[..2] == Keyword::OR => Some((Keyword::OR, Keyword::Or)),
			x if x.len() >= 5 && x[..5] == Keyword::PRINT => Some((Keyword::PRINT, Keyword::Print)),
			x if x.len() >= 6 && x[..6] == Keyword::RETURN => Some((Keyword::RETURN, Keyword::Return)),
			x if x.len() >= 5 && x[..5] == Keyword::SUPER => Some((Keyword::SUPER, Keyword::Super)),
			x if x.len() >= 4 && x[..4] == Keyword::THIS => Some((Keyword::THIS, Keyword::This)),
			x if x.len() >= 4 && x[..4] == Keyword::TRUE => Some((Keyword::TRUE, Keyword::True)),
			x if x.len() >= 3 && x[..3] == Keyword::VAR => Some((Keyword::VAR, Keyword::Var)),
			x if x.len() >= 5 && x[..5] == Keyword::WHILE => Some((Keyword::WHILE, Keyword::While)),
			_ => None,
		};

		let keyword = if let Some((lexeme, const_kw)) = keyword {
			if self.remaining.len() > lexeme.len()
				&& matches!(
					self.remaining[lexeme.len()..lexeme.len() + 1].as_bytes()[0] as char,
					'_' | 'a'..='z' | 'A'..='Z' | '0'..='9'
				)
			{
				None
			} else {
				self.lookahead.character += lexeme.len();

				let span = Span {
					start: self.current,
					end: self.lookahead,
				};
				let token = Token::Keyword(const_kw, span);

				self.remaining = self.remaining.substr(lexeme.len()..);
				self.current = self.lookahead;

				Some(token)
			}
		} else {
			None
		};

		if let Some(token) = keyword {
			return Some(token);
		}

		let result = None
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
