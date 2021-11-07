macro_rules! hashmap {
	($($key:expr => $value:expr),*$(,)?) => {{
		let mut hashmap = ::std::collections::HashMap::new();
		$(hashmap.insert($key, $value);)*

		hashmap
	}}
}

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

criterion_group!(benches, parser);
criterion_main!(benches);

pub fn parser(c: &mut Criterion) {
	let mut group = c.benchmark_group("Parser");
	group.confidence_level(0.99);

	let programs = hashmap![
		"classes.lox" => include_str!("../../test-files/classes.lox"),
		"counter.lox" => include_str!("../../test-files/counter.lox"),
		"fib.lox" => include_str!("../../test-files/fib.lox"),
		"fizzbuzz1.lox" => include_str!("../../test-files/fizzbuzz1.lox"),
		"fizzbuzz2.lox" => include_str!("../../test-files/fizzbuzz2.lox"),
		"forloop.lox" => include_str!("../../test-files/forloop.lox"),
		"fun-expressions.lox" => include_str!("../../test-files/fun-expressions.lox"),
		"fun.lox" => include_str!("../../test-files/fun.lox"),
		"linked-list.lox" => include_str!("../../test-files/linked-list.lox"),
	];

	for (key, program) in programs {
		let name = BenchmarkId::new("derived", key);
		group.bench_with_input(name, program, move |b, input| {
			b.iter_with_large_drop(|| lox::parse(input.into()).unwrap())
		});

		let name = BenchmarkId::new("manual", key);
		group.bench_with_input(name, program, move |b, input| {
			b.iter_with_large_drop(|| lox_rc::parse(input.into()).unwrap())
		});
	}
}
