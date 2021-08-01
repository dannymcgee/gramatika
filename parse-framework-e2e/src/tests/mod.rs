use parse_framework::{self, *};

#[test]
fn it_works() {
	use self::Token::*;

	let input = "let foo = 2 + 2;";
	let mut lexer = TestLexer::new(input);
	let tokens = lexer.scan();

	let expected = vec![
		Keyword("let", span![0:0...0:3]),
		Ident("foo", span![0:4...0:7]),
		Operator("=", span![0:8...0:9]),
		Literal("2", span![0:10...0:11]),
		Operator("+", span![0:12...0:13]),
		Literal("2", span![0:14...0:15]),
		Punct(";", span![0:15...0:16]),
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
		Keyword("let", span![1:0...1:3]),
		Ident("foo", span![1:4...1:7]),
		Operator("=", span![1:8...1:9]),
		Literal("2", span![1:10...1:11]),
		Operator("+", span![1:12...1:13]),
		Literal("2", span![1:14...1:15]),
		Punct(";", span![1:15...1:16]),
		// ...
		Keyword("let", span![2:0...2:3]),
		Ident("bar", span![2:4...2:7]),
		Operator("=", span![2:8...2:9]),
		Ident("foo", span![2:10...2:13]),
		Operator("+", span![2:14...2:15]),
		Ident("foo", span![2:16...2:19]),
		Punct(";", span![2:19...2:20]),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn unterminated_multichar_token() {
	let input = "foo";
	let mut lexer = TestLexer::new(input);
	let tokens = lexer.scan();

	assert_eq!(tokens, vec![Token::Ident("foo", span![0:0...0:3])]);
}

#[test]
fn ident_with_digit() {
	let input = "foo2";
	let mut lexer = TestLexer::new(input);
	let tokens = lexer.scan();

	assert_eq!(tokens, vec![Token::Ident("foo2", span![0:0...0:4])]);
}

#[derive(Debug, PartialEq, Token)]
pub(super) enum Token<'a> {
	#[pattern(r"^(let|var)")]
	Keyword(&'a str, Span),
	#[pattern(r"^[a-zA-Z_][a-zA-Z0-9_]*")]
	Ident(&'a str, Span),
	#[pattern(r"^[;:{}()\[\]]")]
	Punct(&'a str, Span),
	#[pattern(r"^[-+*/=]")]
	Operator(&'a str, Span),
	#[pattern(r"^[0-9]+")]
	Literal(&'a str, Span),
}

type TokenCtor<'a> = fn(&'a str, Span) -> Token<'a>;

pub(super) struct TestLexer<'a> {
	input: &'a str,
	current: Position,
	lookahead: Position,
}

impl<'a> Lexer for TestLexer<'a> {
	type Input = &'a str;
	type Output = Token<'a>;

	fn scan(&mut self) -> Vec<Self::Output> {
		let mut output = vec![];
		while let Some(token) = self.scan_token() {
			output.push(token);
		}

		output
	}

	#[rustfmt::skip]
	fn scan_token(&mut self) -> Option<Self::Output> {
		Token::match_keyword(self.input).map(|m| (m, Token::keyword as TokenCtor))
		.or_else(|| Token::match_ident(self.input).map(|m| (m, Token::ident as TokenCtor)))
		.or_else(|| Token::match_punct(self.input).map(|m| (m, Token::punct as TokenCtor)))
		.or_else(|| Token::match_operator(self.input).map(|m| (m, Token::operator as TokenCtor)))
		.or_else(|| Token::match_literal(self.input).map(|m| (m, Token::literal as TokenCtor)))
		.map(|(m, ctor)| {
			self.lookahead.character += m.end();

			let lexeme = m.as_str();
			let span = Span {
				start: self.current,
				end: self.lookahead,
			};
			let token = ctor(lexeme, span);

			self.input = &self.input[m.end()..];
			self.current = self.lookahead;

			token
		})
		.or_else(|| self.input.chars().peekable().peek().and_then(|c| match c {
			' ' | '\t' | '\r' => {
				self.lookahead.character += 1;
				self.current.character += 1;
				self.input = &self.input[1..];

				self.scan_token()
			},
			'\n' => {
				self.lookahead.line += 1;
				self.lookahead.character = 0;
				self.current = self.lookahead;
				self.input = &self.input[1..];

				self.scan_token()
			},
			other => panic!("Unsupported input: `{}`", other),
		}))
	}
}

impl<'a> TestLexer<'a> {
	fn new(input: &'a str) -> Self {
		Self {
			input,
			current: Position::default(),
			lookahead: Position::default(),
		}
	}
}
