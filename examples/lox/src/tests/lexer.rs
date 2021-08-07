use crate::{Lexer, Token};
use gramatika::Lexer as _;

#[test]
fn it_works() {
	use Token::*;

	let input = "var foo = 2 + 2;";
	let mut lexer = Lexer::new(input);
	let tokens = lexer.scan();

	let expected = vec![
		Keyword("var", span![0:0...0:3]),
		Ident("foo", span![0:4...0:7]),
		Operator("=", span![0:8...0:9]),
		NumLit("2", span![0:10...0:11]),
		Operator("+", span![0:12...0:13]),
		NumLit("2", span![0:14...0:15]),
		Punct(";", span![0:15...0:16]),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn multi_line() {
	use self::Token::*;

	let input = "
var foo = 2 + 2;
var bar = foo + foo;
	";
	let mut lexer = Lexer::new(input);
	let tokens = lexer.scan();

	let expected = vec![
		Keyword("var", span![1:0...1:3]),
		Ident("foo", span![1:4...1:7]),
		Operator("=", span![1:8...1:9]),
		NumLit("2", span![1:10...1:11]),
		Operator("+", span![1:12...1:13]),
		NumLit("2", span![1:14...1:15]),
		Punct(";", span![1:15...1:16]),
		// ...
		Keyword("var", span![2:0...2:3]),
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
fn ident_with_digit() {
	let input = "foo2";
	let mut lexer = Lexer::new(input);
	let tokens = lexer.scan();

	assert_eq!(tokens, vec![Token::Ident("foo2", span![0:0...0:4])]);
}
