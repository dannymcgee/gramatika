#![allow(unused_imports, dead_code)]

#[macro_use]
extern crate gramatika_macro;
#[macro_use]
extern crate gramatika;

use gramatika::{Lexer as _, ParseStreamer, Span, Substr, TokenStream};

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

#[derive(Debug, Token, PartialEq)]
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
}

fn main() {
	let input = "let foo = 2 + 2;";
	let mut lexer = TokenStream::<Token>::new(input.into());
	let tokens = lexer.scan();

	let expected = vec![
		Token::keyword("let".into(), span![1:1..1:4]),
		Token::ident("foo".into(), span![1:5..1:8]),
		Token::operator("=".into(), span![1:9..1:10]),
		Token::literal("2".into(), span![1:11..1:12]),
		Token::operator("+".into(), span![1:13..1:14]),
		Token::literal("2".into(), span![1:15..1:16]),
		Token::punct(";".into(), span![1:16..1:17]),
	];

	assert_eq!(tokens, expected);
}
