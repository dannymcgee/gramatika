use super::{Lexer, Token};
use parse_framework::{span, Lexer as TraitLexer, Token as TraitToken, TokenStream};

#[test]
fn from_iter() {
	use Token::*;

	let tokens = vec![
		Ident("foo", span![0:0...0:3]),
		Ident("bar", span![0:4...0:7]),
		Ident("baz", span![0:8...0:11]),
	];
	let mut stream = tokens.into_iter().collect::<TokenStream<_>>();

	while !stream.is_empty() {
		stream.next();
	}
	assert!(stream.is_empty())
}
