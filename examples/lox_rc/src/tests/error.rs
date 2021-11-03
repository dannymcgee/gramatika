use gramatika::ParseStreamer;

use crate::{parse::ParseStream, stmt::Program};

#[test]
fn error_formatting() {
	expect_err(
		"while true {}",
		r#"
ERROR: Expected `(`
  |
1 | while true {}
  |       ^---
"#,
	);

	expect_err(
		r#"
fun hello(param1, param2, 42) {
	print "That third parameter looks a little strange";
}
"#,
		r#"
ERROR: Expected Ident, found NumLit
  |
2 | fun hello(param1, param2, 42) {
  |                           ^-
"#,
	);

	expect_err(
		"print;",
		r#"
ERROR: Expected expression
  |
1 | print;
  |      ^
"#,
	);

	expect_err(
		r#"
class MultiDigitLineNumber {
	init (foo, bar, baz) {
		this.foo = foo;
		this.bar = bar;
		this.baz = baz;
	}

	printFields() {
		print this.foo;
		print this.bar;
		print this.baz;
	}

	error() {
		This is definitely not valid syntax.
	}
}
"#,
		r#"
ERROR: Expected `;`
   |
16 |         This is definitely not valid syntax.
   |              ^-
"#,
	);
}

fn expect_err(input: &str, expected_message: &str) {
	let err = ParseStream::from(input)
		.parse::<Program>()
		.unwrap_err()
		.to_string();

	if expected_message != err {
		eprintln!("{}", err);
		panic!();
	}
	// assert_eq!(expected_message, format!("{}", err));
}
