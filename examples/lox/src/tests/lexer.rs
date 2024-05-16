use crate::{tokens::Token, TokenStream};
use gramatika::{arcstr::literal_substr, Lexer as _, Substr};

#[test]
fn it_works() {
	use Token::*;

	let input = "var foo = 2 + 2;";
	let mut lexer = TokenStream::new(Substr::from(input));
	let tokens = lexer.scan();

	let expected = vec![
		Keyword(literal_substr!("var"), span![1:1..1:4]),
		Ident(literal_substr!("foo"), span![1:5..1:8]),
		Operator(literal_substr!("="), span![1:9..1:10]),
		NumLit(literal_substr!("2"), span![1:11..1:12]),
		Operator(literal_substr!("+"), span![1:13..1:14]),
		NumLit(literal_substr!("2"), span![1:15..1:16]),
		Punct(literal_substr!(";"), span![1:16..1:17]),
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
	let mut lexer = TokenStream::new(Substr::from(input));
	let tokens = lexer.scan();

	let expected = vec![
		Keyword(literal_substr!("var"), span![2:1..2:4]),
		Ident(literal_substr!("foo"), span![2:5..2:8]),
		Operator(literal_substr!("="), span![2:9..2:10]),
		NumLit(literal_substr!("2"), span![2:11..2:12]),
		Operator(literal_substr!("+"), span![2:13..2:14]),
		NumLit(literal_substr!("2"), span![2:15..2:16]),
		Punct(literal_substr!(";"), span![2:16..2:17]),
		// ...
		Keyword(literal_substr!("var"), span![3:1..3:4]),
		Ident(literal_substr!("bar"), span![3:5..3:8]),
		Operator(literal_substr!("="), span![3:9..3:10]),
		Ident(literal_substr!("foo"), span![3:11..3:14]),
		Operator(literal_substr!("+"), span![3:15..3:16]),
		Ident(literal_substr!("foo"), span![3:17..3:20]),
		Punct(literal_substr!(";"), span![3:20..3:21]),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn multi_line_token() {
	use self::Token::*;

	let input = "
/**
 * Here's a block comment!
 */
var foo = 2 + 2;
	";
	let mut lexer = TokenStream::new(input.into());
	let tokens = lexer.scan();

	let expected = vec![
		Keyword(literal_substr!("var"), span![5:1..5:4]),
		Ident(literal_substr!("foo"), span![5:5..5:8]),
		Operator(literal_substr!("="), span![5:9..5:10]),
		NumLit(literal_substr!("2"), span![5:11..5:12]),
		Operator(literal_substr!("+"), span![5:13..5:14]),
		NumLit(literal_substr!("2"), span![5:15..5:16]),
		Punct(literal_substr!(";"), span![5:16..5:17]),
	];

	assert_eq!(tokens, expected);
}

#[test]
fn ident_with_digit() {
	let input = "foo2";
	let mut lexer = TokenStream::new(Substr::from(input));
	let tokens = lexer.scan();

	assert_eq!(
		tokens,
		vec![Token::Ident(literal_substr!("foo2"), span![1:1..1:5])]
	);
}
