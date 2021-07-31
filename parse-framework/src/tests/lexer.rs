use super::*;

#[test]
fn it_works() {
	use self::Token::*;

	let input = "let foo = 2 + 2;";
	let mut lexer = TestLexer::new(input);
	let tokens = lexer.scan();

	let expected = vec![
		Keyword("let", Span::new((0, 0), (0, 3))),
		Ident("foo", Span::new((0, 4), (0, 7))),
		Operator("=", Span::new((0, 8), (0, 9))),
		Literal("2", Span::new((0, 10), (0, 11))),
		Operator("+", Span::new((0, 12), (0, 13))),
		Literal("2", Span::new((0, 14), (0, 15))),
		Punct(";", Span::new((0, 15), (0, 16))),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn multi_line() {
	use self::Token::*;

	let input = "
let foo = 2 + 2;
let bar = foo + foo;
	";
	let mut lexer = TestLexer::new(input);
	let tokens = lexer.scan();

	let expected = vec![
		Keyword("let", Span::new((1, 0), (1, 3))),
		Ident("foo", Span::new((1, 4), (1, 7))),
		Operator("=", Span::new((1, 8), (1, 9))),
		Literal("2", Span::new((1, 10), (1, 11))),
		Operator("+", Span::new((1, 12), (1, 13))),
		Literal("2", Span::new((1, 14), (1, 15))),
		Punct(";", Span::new((1, 15), (1, 16))),
		// ...
		Keyword("let", Span::new((2, 0), (2, 3))),
		Ident("bar", Span::new((2, 4), (2, 7))),
		Operator("=", Span::new((2, 8), (2, 9))),
		Ident("foo", Span::new((2, 10), (2, 13))),
		Operator("+", Span::new((2, 14), (2, 15))),
		Ident("foo", Span::new((2, 16), (2, 19))),
		Punct(";", Span::new((2, 19), (2, 20))),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn unterminated_multichar_token() {
	let input = "foo";
	let mut lexer = TestLexer::new(input);
	let tokens = lexer.scan();

	assert_eq!(tokens, vec![Token::Ident("foo", Span::new((0, 0), (0, 3)))]);
}

#[test]
fn ident_with_digit() {
	let input = "foo2";
	let mut lexer = TestLexer::new(input);
	let tokens = lexer.scan();

	assert_eq!(
		tokens,
		vec![Token::Ident("foo2", Span::new((0, 0), (0, 4)))]
	);
}

#[derive(Debug, PartialEq)]
pub(super) enum Token<'a> {
	Ident(&'a str, Span),
	Keyword(&'a str, Span),
	Punct(&'a str, Span),
	Operator(&'a str, Span),
	Literal(&'a str, Span),
}

impl<'a> Token<'a> {
	pub fn as_inner(&'a self) -> (&'a str, Span) {
		use self::Token::*;

		match self {
			Ident(lexeme, span) => (*lexeme, *span),
			Keyword(lexeme, span) => (*lexeme, *span),
			Punct(lexeme, span) => (*lexeme, *span),
			Operator(lexeme, span) => (*lexeme, *span),
			Literal(lexeme, span) => (*lexeme, *span),
		}
	}

	fn ident(lexeme: &'a str, span: Span) -> Self {
		Self::Ident(lexeme, span)
	}
	fn keyword(lexeme: &'a str, span: Span) -> Self {
		Self::Keyword(lexeme, span)
	}
	fn punct(lexeme: &'a str, span: Span) -> Self {
		Self::Punct(lexeme, span)
	}
	fn operator(lexeme: &'a str, span: Span) -> Self {
		Self::Operator(lexeme, span)
	}
	fn literal(lexeme: &'a str, span: Span) -> Self {
		Self::Literal(lexeme, span)
	}
}

impl<'a> super::Token for Token<'a> {
	fn span(&self) -> Span {
		self.as_inner().1
	}

	fn lexeme(&self) -> &str {
		self.as_inner().0
	}
}

pub(super) struct TestLexer<'a> {
	input: &'a str,
	current: Position,
	lookahead: Position,
	next_token: fn(&'a str, Span) -> Token<'a>,
}

impl<'a> Lexer for TestLexer<'a> {
	type Input = &'a str;
	type Output = Token<'a>;

	fn new(input: &'a str) -> Self {
		Self {
			input,
			current: Position::default(),
			lookahead: Position::default(),
			next_token: Token::ident,
		}
	}

	fn scan(&mut self) -> Vec<Self::Output> {
		let mut output = vec![];
		while let Some(token) = self.scan_token() {
			output.push(token);
		}

		output
	}

	fn scan_token(&mut self) -> Option<Self::Output> {
		if self.identify().is_ok() {
			let (lexeme, rest) = self
				.input
				.split_at(self.lookahead.character - self.current.character);

			let span = Span {
				start: self.current,
				end: self.lookahead,
			};

			if matches!(lexeme, "let" | "var") {
				self.next_token = Token::keyword;
			}

			let token = (self.next_token)(lexeme, span);

			self.input = rest;
			self.current = self.lookahead;

			Some(token)
		} else {
			None
		}
	}
}

impl<'a> TestLexer<'a> {
	fn identify(&mut self) -> Result<(), ()> {
		let mut chars = self.input.chars().peekable();

		if let Some(c) = chars.peek() {
			match c {
				'+' | '-' | '*' | '/' | '=' => {
					self.next_token = Token::operator;
					self.lookahead.character += 1;
				}
				'0'..='9' => {
					self.next_token = Token::literal;
					let (end, _) = chars
						.enumerate()
						.find(|(_, c)| !matches!(c, '0'..='9'))
						.unwrap_or_else(|| {
							(
								self.input.chars().count(),
								self.input.chars().last().unwrap(),
							)
						});

					self.lookahead.character += end;
				}
				'a'..='z' | 'A'..='Z' | '_' => {
					self.next_token = Token::ident;
					let (end, _) = chars
						.enumerate()
						.find(
							|(_, c)| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'),
						)
						.unwrap_or_else(|| {
							(
								self.input.chars().count(),
								self.input.chars().last().unwrap(),
							)
						});

					self.lookahead.character += end;
				}
				';' => {
					self.next_token = Token::punct;
					self.lookahead.character += 1;
				}
				' ' | '\t' | '\r' => {
					self.lookahead.character += 1;
					self.current.character += 1;
					self.input = &self.input[1..];

					return self.identify();
				}
				'\n' => {
					self.lookahead.line += 1;
					self.lookahead.character = 0;
					self.current = self.lookahead;
					self.input = &self.input[1..];

					return self.identify();
				}
				other => {
					panic!("No matcher for '{}'", other);
				}
			}

			Ok(())
		} else {
			Err(())
		}
	}
}
