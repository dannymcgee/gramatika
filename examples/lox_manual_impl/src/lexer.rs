use arcstr::{ArcStr, Substr};
use gramatika::{Position, Span};

use crate::tokens::{Keyword, Token};

pub struct Lexer {
	input: ArcStr,
	remaining: Substr,
	current: Position,
	lookahead: Position,
}

/// (Note: doc-comment purely for the sake of syntax highlighting)
/// ```
/// let mut len = 0_usize;
/// let mut keyword: Option<Keyword> = None;
/// let mut iter = self.remaining.char_indices();
///
/// while let Some((i, c)) = iter.next() {
///    // Recursuve -- not really viable for macro_rules!
///    match c {
///       'a' => match iter.next() {
///          Some((_, 'n')) => match iter.next() {
///             Some((_, 'd')) => {
///                 keyword = Some(Keyword::And);
///                 len = 3;
///             }
///             Some((i, _)) => {
///                 keyword = None;
///                 len = i;
///             }
///             None => break,
///          }
///          Some((i, _)) => {
///             keyword = None;
///             len = i;
///          }
///          None => break,
///       }
///    }
///
///    // Ugly, but just might work!
///    match c {
///       'a' => {
///          len += 1;
///          keyword = Some(Keyword::And);
///
///          // macro iteration...
///          let next = iter.next();
///          if next.is_none() {
///             break;
///          }
///          len += 1;
///
///          if !matches!(next, Some((_, 'n'))) {
///             keyword = None;
///
///             if !matches!(next, Some((_, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))) {
///                 len -= 1;
///             }
///             continue;
///          }
///
///          // macro iteration...
///          let next = iter.next();
///          if next.is_none() {
///             break;
///          }
///          len += 1;
///
///          if !matches!(next, Some((_, 'd'))) {
///             keyword = None;
///
///             if !matches!(next, Some((_, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))) {
///                 len -= 1;
///             }
///             continue;
///          }
///
///          // end macro iterations
///          len = $macro_var;
///       }
///    }
/// }
/// ```
macro_rules! try_scan {
	(
		$( $char:literal )+, {
			$keyword:ident = $variant:ident;
			$len:ident = $len_val:literal;
		} ($i:ident, $iter:ident)
	) => {{
		$len += 1;
		$keyword = Some(Keyword::$variant);

		$(
			let next = $iter.next();
			if next.is_none() {
				break;
			}
			$len += 1;

			if !matches!(next, Some((_, $char))) {
				$keyword = None;

				if !matches!(next, Some((_, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))) {
					$len -= 1;
				}
				continue;
			}
		)+

		$len = $len_val;
	}};
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

	#[rustfmt::skip]
	fn scan_keyword_or_ident(&mut self) -> Option<Token> {
		let mut len = 0_usize;
		let mut keyword: Option<Keyword> = None;
		let mut iter = self.remaining.char_indices();

		while let Some((i, c)) = iter.next() {
			match (i, c) {
				(0, 'a') => try_scan!('n''d', {
					keyword = And;
					len = 3;
				} (i, iter)),

				(0, 'c') => try_scan!('l''a''s''s', {
					keyword = Class;
					len = 5;
				} (i, iter)),

				(0, 'e') => try_scan!('l''s''e', {
					keyword = Else;
					len = 4;
				} (i, iter)),

				(0, 'f') => {
					len += 1;

					match iter.next() {
						Some((_, 'o')) => try_scan!('r', {
							keyword = For;
							len = 3;
						} (i, iter)),

						Some((_, 'u')) => try_scan!('n', {
							keyword = Fun;
							len = 3;
						} (i, iter)),

						Some((_, 'a')) => try_scan!('l''s''e', {
							keyword = False;
							len = 5;
						} (i, iter)),

						Some((_, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')) => {
							keyword = None;
							len += 1;
						}

						_ => break,
					}
				}

				(0, 'i') => try_scan!('f', {
					keyword = If;
					len = 2;
				} (i, iter)),

				(0, 'n') => try_scan!('i''l', {
					keyword = Nil;
					len = 3;
				 } (i, iter)),

				(0, 'o') => try_scan!('r', {
					keyword = Or;
					len = 2;
				} (i, iter)),

				(0, 'p') => try_scan!('r''i''n''t', {
					keyword = Print;
					len = 5;
				} (i, iter)),

				(0, 'r') => try_scan!('e''t''u''r''n', {
					keyword = Return;
					len = 6;
				} (i, iter)),

				(0, 's') => try_scan!('u''p''e''r', {
					keyword = Super;
					len = 5;
				} (i, iter)),

				(0, 'v') => try_scan!('a''r', {
					keyword = Var;
					len = 3;
				} (i, iter)),

				(0, 'w') => try_scan!('h''i''l''e', {
					keyword = While;
					len = 5;
				} (i, iter)),

				(0, 'a'..='z' | 'A'..='Z' | '_') => {
					keyword = None;
					len += 1;
				}

				((1..), 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') => {
					keyword = None;
					len += 1;
				}

				_ => break,
			}
		}

		if len == 0 {
			None
		} else {
			self.lookahead.character += len;
			let span = Span {
				start: self.current,
				end: self.lookahead,
			};

			let token = match keyword {
				None => Token::Ident(self.remaining.substr(..len), span),
				Some(kw) => Token::Keyword(kw, span),
			};

			self.remaining = self.remaining.substr(len..);
			self.current = self.lookahead;

			Some(token)
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
		if let Some(token) = self.scan_keyword_or_ident() {
			return Some(token);
		}

		let result = None
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
