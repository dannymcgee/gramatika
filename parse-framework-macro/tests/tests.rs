use trybuild::TestCases;

#[test]
fn tests() {
	let t = TestCases::new();
	t.pass("tests/01-setup.rs");
	t.pass("tests/02-ctor-methods.rs");
	t.pass("tests/03-as-inner.rs");
	t.pass("tests/04-trait-impl.rs");
}
