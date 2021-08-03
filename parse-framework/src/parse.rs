use std::{collections::VecDeque, fmt, iter::FromIterator};

use crate::Token;

pub trait Parse {
	type Token: crate::Token;

	fn parse(input: &mut TokenStream<Self::Token>) -> Result<Self, String>
	where Self: Sized;
}

#[derive(Clone, Debug, PartialEq)]
pub struct TokenStream<T>
where T: Token
{
	inner: VecDeque<T>,
}

impl<T> Default for TokenStream<T>
where T: Token
{
	fn default() -> Self {
		Self {
			inner: VecDeque::new(),
		}
	}
}

impl<T> TokenStream<T>
where T: Token + fmt::Debug
{
	pub fn new() -> Self {
		Self::default()
	}

	pub fn parse<P: Parse<Token = T>>(&mut self) -> Result<P, String> {
		P::parse(self)
	}

	pub fn len(&self) -> usize {
		self.inner.len()
	}

	pub fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub fn peek(&self) -> Option<&T> {
		self.inner.front()
	}

	pub fn skip(&mut self) {
		self.inner.pop_front();
	}

	pub fn check_kind(&self, kind: T::Kind) -> bool {
		self.peek().map(|tok| kind == tok.kind()).unwrap_or(false)
	}

	pub fn check(&self, compare: T) -> bool {
		self.peek()
			.map(|peek| {
				peek.kind() == compare.kind() && peek.lexeme() == compare.lexeme()
			})
			.unwrap_or(false)
	}

	pub fn take(&mut self, compare: T) -> Result<T, String> {
		self.next()
			.map(|token| {
				if token.kind() == compare.kind() && token.lexeme() == compare.lexeme() {
					Ok(token)
				} else {
					let detail = if token.kind() != compare.kind() {
						format!("Kind {:?} != {:?}", token.kind(), compare.kind())
					} else {
						format!("Lexeme `{}` != `{}`", token.lexeme(), compare.lexeme())
					};

					Err(format!(
						"Expected {:?} `{}` but found {:?}\n{}",
						compare.kind(),
						compare.lexeme(),
						token,
						detail,
					))
				}
			})
			.unwrap_or_else(|| Err("Unexpected end of input".into()))
	}

	pub fn take_kind(&mut self, kind: T::Kind) -> Result<T, String> {
		self.next()
			.map(|token| {
				if token.kind() == kind {
					Ok(token)
				} else {
					Err(format!("Expected {:?} but found {:?}", kind, token))
				}
			})
			.unwrap_or_else(|| Err("End of file".into()))
	}
}

impl<T> FromIterator<T> for TokenStream<T>
where T: Token
{
	fn from_iter<I>(iter: I) -> Self
	where I: IntoIterator<Item = T> {
		let mut iter = iter.into_iter();
		let mut inner = match iter.next() {
			None => VecDeque::new(),
			Some(element) => {
				let (lower, _) = iter.size_hint();
				let mut vec = VecDeque::with_capacity(lower.saturating_add(1));
				vec.push_back(element);
				vec
			}
		};

		inner.extend(iter);

		Self { inner }
	}
}

impl<T> Iterator for TokenStream<T>
where T: Token
{
	type Item = T;

	fn next(&mut self) -> Option<Self::Item> {
		self.inner.pop_front()
	}
}
