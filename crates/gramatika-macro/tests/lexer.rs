#![allow(unused_imports, dead_code)]

#[macro_use]
extern crate gramatika_macro;
#[macro_use]
extern crate gramatika;

use gramatika::{Lexer as _, ParseStreamer, Span, Substr};

/// Expected output:
///
/// ```
/// struct Lexer {
///     input: ::gramatika::Substr,
///     remaining: ::gramatika::Substr,
///     current: ::gramatika::Position,
///     lookahead: ::gramatika::Position,
/// }
///
/// impl Lexer {
///     fn new(input: ::gramatika::Substr) -> Self {
///         Self {
///             input,
///             remaining: input,
///             current: ::gramatika::Position::default(),
///             lookahead: ::gramatika::Position::default(),
///         }
///     }
/// }
///
/// type __TOKEN_CTOR = fn(::gramatika::Substr, ::gramatika::Span) -> Token;
///
/// impl ::gramatika::Lexer for Lexer {
///     type Output = Token;
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
///         let result = None
///             .or_else(|| Token::match_keyword(&self.remaining).map(|m| (m, Token::keyword as __TOKEN_CTOR)))
///             .or_else(|| Token::match_ident(&self.remaining).map(|m| (m, Token::ident as __TOKEN_CTOR)))
///             .or_else(|| Token::match_punct(&self.remaining).map(|m| (m, Token::punct as __TOKEN_CTOR)))
///             .or_else(|| Token::match_operator(&self.remaining).map(|m| (m, Token::operator as __TOKEN_CTOR)))
///             .or_else(|| Token::match_literal(&self.remaining).map(|m| (m, Token::literal as __TOKEN_CTOR)));
///
///         match result {
///             Some((m, ctor)) => {
///                 let match_end = m.end();
///                 let lexeme = self.remaining.substr_from(m.as_str());
///
///                 self.lookahead.character += match_end;
///
///                 let span = Span {
///                     start: self.current,
///                     end: self.lookahead,
///                 };
///                 let token = ctor(lexeme, span);
///
///                 self.remaining = self.remaining.substr(match_end..);
///                 self.current = self.lookahead;
///
///                 Some(token)
///             }
///             None => {
///                 self.remaining.clone().chars().next().and_then(|c| match c {
///                     ' ' | '\t' | '\r' => {
///                         self.lookahead.character += 1;
///                         self.current.character += 1;
///                         self.remaining = self.remaining.substr(1..);
///
///                         self.scan_token()
///                     }
///                     '\n' => {
///                         self.lookahead.line += 1;
///                         self.lookahead.character = 0;
///                         self.current = self.lookahead;
///                         self.remaining = self.remaining.substr(1..);
///
///                         self.scan_token()
///                     }
///                     other => panic!("Unsupported input: `{}` at {:?}", other, self.current),
///                 })
///             }
///         }
///     }
/// }
/// ```

// ...

#[derive(Debug, Token, Lexer, PartialEq)]
enum Token {
	#[subset_of(Ident)]
	#[pattern = "let|var|if|for|while|return"]
	Keyword(Substr, Span),

	#[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	Ident(Substr, Span),

	#[pattern = r"[;:{}()\[\]]"]
	Punct(Substr, Span),

	#[pattern = "[-+*/=]"]
	Operator(Substr, Span),

	#[pattern = "[0-9]+"]
	Literal(Substr, Span),

	OverrideTest(Substr, Span),
}

fn main() {
	let input = "let foo = 2 + 2;";
	let mut lexer = Lexer::new(input.into()).with_runtime_matcher(|s| {
		if s.len() >= 3 && &s[..3] == "foo" {
			Some((3, TokenKind::OverrideTest))
		} else {
			None
		}
	});
	let tokens = lexer.scan();

	let expected = vec![
		Token::keyword("let".into(), span![0:0...0:3]),
		Token::override_test("foo".into(), span![0:4...0:7]),
		Token::operator("=".into(), span![0:8...0:9]),
		Token::literal("2".into(), span![0:10...0:11]),
		Token::operator("+".into(), span![0:12...0:13]),
		Token::literal("2".into(), span![0:14...0:15]),
		Token::punct(";".into(), span![0:15...0:16]),
	];

	assert_eq!(tokens, expected);
}
