use crate::Token;

pub trait Lexer {
	type Input;
	type Output: Token;

	fn scan(&mut self) -> Vec<Self::Output>;
	fn scan_token(&mut self) -> Option<Self::Output>;
}
