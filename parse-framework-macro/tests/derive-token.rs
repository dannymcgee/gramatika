use parse_framework::{span, Span, Token as TokenTrait};

#[macro_use]
extern crate parse_framework_macro;

#[derive(Debug, Token)]
enum Token<'a> {
	Keyword(&'a str, Span),
	Ident(&'a str, Span),
	Punct(&'a str, Span),
	Operator(&'a str, Span),
	Literal(&'a str, Span),
}

fn main() {
	let keyword = Token::keyword("let", span![0:0...0:3]);
	let ident = Token::ident("foo", span![0:4...0:7]);
	let equal = Token::operator("=", span![0:8...0:9]);
	let one = Token::literal("1", span![0:10...0:11]);
	let plus = Token::operator("+", span![0:12...0:13]);
	let two = Token::literal("2", span![0:14...0:15]);
	let terminator = Token::punct(";", span![0:15...0:16]);

	eprintln!("keyword: {:?}", keyword);
	eprintln!("ident: {:?}", ident);
	eprintln!("equal: {:?}", equal);
	eprintln!("one: {:?}", one);
	eprintln!("plus: {:?}", plus);
	eprintln!("two: {:?}", two);
	eprintln!("terminator: {:?}", terminator);

	eprintln!(
		"{} {} {} {} {} {}{}",
		keyword.lexeme(),
		ident.lexeme(),
		equal.lexeme(),
		one.lexeme(),
		plus.lexeme(),
		two.lexeme(),
		terminator.lexeme(),
	);
}
