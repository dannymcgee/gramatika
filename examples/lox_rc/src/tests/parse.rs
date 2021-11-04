use gramatika::{Lexer as _, ParseStreamer};

use crate::{parse::ParseStream, stmt::Program, Lexer};

#[test]
fn print_stmt() {
	parse(r#"print "Hello, world!";"#);
	parse("print true;");
	parse("print 2 + 1;");
	parse(r#"print "foo" + "bar";"#);
	parse(r#"print "The answer" == 42;"#);
	parse("print nil;");
}

#[test]
fn class_decl() {
	parse(
		r#"
class Breakfast {
	cook() {
		print "Eggs a-fryin'!";
	}
	serve(who) {
		print "Enjoy your breakfast, " + who + ".";
	}
}
		"#,
	);
}

#[test]
fn class_decl_w_ctor() {
	parse(
		r#"
class Breakfast {
	init (meat, bread) {
		this.meat = meat;
		this.bread = bread;
	}

	serve(who) {
		print "Enjoy your " + this.meat + " and " + this.bread + ", " + who + ".";
	}
}
		"#,
	);
}

#[test]
fn class_decl_w_super() {
	parse(
		r#"
class Brunch < Breakfast {
	init (meat, bread, drink) {
		super.init(meat, bread);
		this.drink = drink;
	}

	serve(who) {
		super.serve(who);
		print "How about a " + this.drink + " to wash it down?";
	}
}
		"#,
	);
}

#[test]
fn fun_decl() {
	parse(
		r#"
fun printSum(a, b) {
	print a + b;
}

fun sum(a, b) {
	return a + b;
}
		"#,
	);
}

#[test]
fn var_decl() {
	parse(
		r#"
var theAnswer = 42;
		"#,
	);
}

#[test]
fn lambda() {
	parse(
		r#"
var sum = fun (a, b) {
	return a + b;
};
		"#,
	);
}

#[test]
fn higher_order_funs() {
	parse(
		r#"
fun higherOrder(func) {
	func();
}

higherOrder(fun () {
	print "Hello!";
});
		"#,
	);
}

#[test]
fn if_stmt() {
	parse(
		r#"
if (condition) {
	print "yes";
} else if (otherCondition) {
	print "maybe";
} else {
	print "no";
}
		"#,
	);
}

#[test]
fn while_stmt() {
	parse(
		r#"
var a = 1;
while (a < 10) {
	print a;
	a = a + 1;
}
		"#,
	);
}

#[test]
fn for_stmt() {
	parse(
		r#"
for (var i = 0; i < 10; i = i + 1) {
	print i;
}
		"#,
	);
}

#[test]
fn test_programs() {
	parse(include_str!("../../../../test-files/classes.lox"));
	parse(include_str!("../../../../test-files/counter.lox"));
	parse(include_str!("../../../../test-files/fib.lox"));
	parse(include_str!("../../../../test-files/fizzbuzz1.lox"));
	parse(include_str!("../../../../test-files/fizzbuzz2.lox"));
	parse(include_str!("../../../../test-files/forloop.lox"));
	parse(include_str!("../../../../test-files/fun-expressions.lox"));
	parse(include_str!("../../../../test-files/fun.lox"));
	parse(include_str!("../../../../test-files/linked-list.lox"));
}

fn parse(input: &str) {
	let mut parser = ParseStream::from(input);

	match parser.parse::<Program>() {
		Ok(_) => {}
		Err(err) => {
			let mut lexer = Lexer::new(input.into());

			eprintln!("{:#?}", lexer.scan());
			eprintln!("{}", err);
			panic!();
		}
	}
}
