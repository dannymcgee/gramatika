use crate::{lexer::Lexer, tokens::Token};
use gramatika::Lexer as _;

macro_rules! span {
	($start_line:literal:$start_char:literal..$end_line:literal:$end_char:literal) => {
		::gramatika::Span::new(
			($start_line - 1, $start_char - 1),
			($end_line - 1, $end_char - 1),
		)
	};
}

#[test]
fn it_works() {
	let input = "var foo = 2 + 2;";
	let mut lexer = Lexer::new(input.into());
	let tokens = lexer.scan();

	let expected = vec![
		Token::Keyword("var".into(), span![1:1..1:4]),
		Token::Ident("foo".into(), span![1:5..1:8]),
		Token::Operator("=".into(), span![1:9..1:10]),
		Token::NumLit("2".into(), span![1:11..1:12]),
		Token::Operator("+".into(), span![1:13..1:14]),
		Token::NumLit("2".into(), span![1:15..1:16]),
		Token::Punct(";".into(), span![1:16..1:17]),
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
		Token::Keyword("var".into(), span![2:1..2:4]),
		Token::Ident("foo".into(), span![2:5..2:8]),
		Token::Operator("=".into(), span![2:9..2:10]),
		Token::NumLit("2".into(), span![2:11..2:12]),
		Token::Operator("+".into(), span![2:13..2:14]),
		Token::NumLit("2".into(), span![2:15..2:16]),
		Token::Punct(";".into(), span![2:16..2:17]),
		// ...
		Token::Keyword("var".into(), span![3:1..3:4]),
		Token::Ident("bar".into(), span![3:5..3:8]),
		Token::Operator("=".into(), span![3:9..3:10]),
		Token::Ident("foo".into(), span![3:11..3:14]),
		Token::Operator("+".into(), span![3:15..3:16]),
		Token::Ident("foo".into(), span![3:17..3:20]),
		Token::Punct(";".into(), span![3:20..3:21]),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn multi_line_token() {
	let input = "
/**
 * Here's a block comment!
 */
var foo = 2 + 2;
	";
	let mut lexer = Lexer::new(input.into());
	let tokens = lexer.scan();

	let expected = vec![
		Token::Keyword("var".into(), span![5:1..5:4]),
		Token::Ident("foo".into(), span![5:5..5:8]),
		Token::Operator("=".into(), span![5:9..5:10]),
		Token::NumLit("2".into(), span![5:11..5:12]),
		Token::Operator("+".into(), span![5:13..5:14]),
		Token::NumLit("2".into(), span![5:15..5:16]),
		Token::Punct(";".into(), span![5:16..5:17]),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn ident_with_digit() {
	let input = "foo2";
	let mut lexer = Lexer::new(input.into());
	let tokens = lexer.scan();

	assert_eq!(tokens, vec![Token::Ident("foo2".into(), span![1:1..1:5])]);
}
