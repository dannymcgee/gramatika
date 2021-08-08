#![allow(unused_imports, dead_code)]

#[macro_use]
extern crate gramatika_macro;
#[macro_use]
extern crate gramatika;

use gramatika::{Lexer as _, ParseStreamer, Span};

/// Expected output:
///
/// ```
/// struct Lexer<'a> {
///     input: &'a str,
///     remaining: &'a str,
///     current: ::gramatika::Position,
///     lookahead: ::gramatika::Position,
/// }
///
/// impl<'a> Lexer<'a> {
///     fn new(input: &'a str) -> Self {
///         Self {
///             input,
///             remaining: input,
///             current: ::gramatika::Position::default(),
///             lookahead: ::gramatika::Position::default(),
///         }
///     }
/// }
///
/// type __TOKEN_CTOR<'a> = fn(&'a str, ::gramatika::Span) -> Token<'a>;
///
/// impl<'a> ::gramatika::Lexer for Lexer<'a> {
///     type Input = &'a str;
///     type Output = Token<'a>;
///
///     fn scan(&mut self) -> Vec<Self::Output> {
///         let mut output = vec![];
///         while let Some(token) = self.scan_token() {
///             output.push(token);
///         }
///
///         output
///     }
///
///     fn scan_token(&mut self) -> Option<Self::Output> {
///         None.or_else(|| Token::match_keyword(self.input).map(|m| (m, Token::keyword as __TOKEN_CTOR)))
///             .or_else(|| Token::match_ident(self.input).map(|m| (m, Token::ident as __TOKEN_CTOR)))
///             .or_else(|| Token::match_punct(self.input).map(|m| (m, Token::punct as __TOKEN_CTOR)))
///             .or_else(|| Token::match_operator(self.input).map(|m| (m, Token::operator as __TOKEN_CTOR)))
///             .or_else(|| Token::match_literal(self.input).map(|m| (m, Token::literal as __TOKEN_CTOR)))
///             .map(|(m, ctor)| {
///                 self.lookahead.character += m.end();
///
///                 let lexeme = m.as_str();
///                 let span = ::gramatika::Span {
///                     start: self.current,
///                     end: self.lookahead,
///                 };
///                 let token = ctor(lexeme, span);
///
///                 self.remaining = &self.remaining[m.end()..];
///                 self.current = self.lookahead;
///
///                 token
///             })
///             .or_else(|| self.remaining.chars().peekable().peek().and_then(|c| match c {
///                 ' ' | '\t' | '\r' => {
///                     self.lookahead.character += 1;
///                     self.current.character += 1;
///                     self.remaining = &self.remaining[1..];
///
///                     self.scan_token()
///                 },
///                 '\n' => {
///                     self.lookahead.line += 1;
///                     self.lookahead.character = 0;
///                     self.current = self.lookahead;
///                     self.remaining = &self.remaining[1..];
///
///                     self.scan_token()
///                 },
///                 other => panic!("Unsupported input: `{}`", other),
///             }))
///     }
/// }
/// ```

// ...

#[derive(Debug, Token, Lexer, PartialEq)]
enum Token<'a> {
	#[pattern = "let|var|if|for|while|return"]
	Keyword(&'a str, Span),

	#[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(&'a str, Span),

	#[pattern = r"[;:{}()\[\]]"]
	Punct(&'a str, Span),

	#[pattern = "[-+*/=]"]
	Operator(&'a str, Span),

	#[pattern = "[0-9]+"]
	Literal(&'a str, Span),
}

fn main() {
	let input = "let foo = 2 + 2;";
	let mut lexer = Lexer::new(input);
	let tokens = lexer.scan();

	let expected = vec![
		Token::keyword("let", span![0:0...0:3]),
		Token::ident("foo", span![0:4...0:7]),
		Token::operator("=", span![0:8...0:9]),
		Token::literal("2", span![0:10...0:11]),
		Token::operator("+", span![0:12...0:13]),
		Token::literal("2", span![0:14...0:15]),
		Token::punct(";", span![0:15...0:16]),
	];

	assert_eq!(tokens, expected);
}
