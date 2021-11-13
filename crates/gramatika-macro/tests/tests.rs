use trybuild::TestCases;

#[test]
fn tests() {
	let t = TestCases::new();
	t.pass("tests/debug_lisp.rs");
	t.pass("tests/token.rs");
	t.pass("tests/pattern.rs");
	t.pass("tests/lexer.rs");
	t.pass("tests/regex.rs");
}
