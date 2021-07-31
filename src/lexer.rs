use crate::Token;

pub trait Lexer {
	type Input;
	type Output: Token;

	fn new(input: Self::Input) -> Self;
	fn scan(&mut self) -> Vec<Self::Output>;
	fn scan_token(&mut self) -> Option<Self::Output>;
}
