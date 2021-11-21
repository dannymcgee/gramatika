use crate::{lexer::Lexer, tokens::Token};
use gramatika::Lexer as _;

macro_rules! span {
	($start_line:literal:$start_char:literal...$end_line:literal:$end_char:literal) => {
		::gramatika::Span::new(($start_line, $start_char), ($end_line, $end_char))
	};
}

#[test]
fn it_works() {
	let input = "var foo = 2 + 2;";
	let mut lexer = Lexer::new(input.into());
	let tokens = lexer.scan();

	let expected = vec![
		Token::Keyword("var".into(), span![0:0...0:3]),
		Token::Ident("foo".into(), span![0:4...0:7]),
		Token::Operator("=".into(), span![0:8...0:9]),
		Token::NumLit("2".into(), span![0:10...0:11]),
		Token::Operator("+".into(), span![0:12...0:13]),
		Token::NumLit("2".into(), span![0:14...0:15]),
		Token::Punct(";".into(), span![0:15...0:16]),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn multi_line() {
	let input = "
var foo = 2 + 2;
var bar = foo + foo;
	";
	let mut lexer = Lexer::new(input.into());
	let tokens = lexer.scan();

	let expected = vec![
		Token::Keyword("var".into(), span![1:0...1:3]),
		Token::Ident("foo".into(), span![1:4...1:7]),
		Token::Operator("=".into(), span![1:8...1:9]),
		Token::NumLit("2".into(), span![1:10...1:11]),
		Token::Operator("+".into(), span![1:12...1:13]),
		Token::NumLit("2".into(), span![1:14...1:15]),
		Token::Punct(";".into(), span![1:15...1:16]),
		// ...
		Token::Keyword("var".into(), span![2:0...2:3]),
		Token::Ident("bar".into(), span![2:4...2:7]),
		Token::Operator("=".into(), span![2:8...2:9]),
		Token::Ident("foo".into(), span![2:10...2:13]),
		Token::Operator("+".into(), span![2:14...2:15]),
		Token::Ident("foo".into(), span![2:16...2:19]),
		Token::Punct(";".into(), span![2:19...2:20]),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn ident_with_digit() {
	let input = "foo2";
	let mut lexer = Lexer::new(input.into());
	let tokens = lexer.scan();

	assert_eq!(tokens, vec![Token::Ident("foo2".into(), span![0:0...0:4])]);
}
