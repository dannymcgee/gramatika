# Gramatika
A minimal toolkit for writing parsers with Rust

## Motivation
Though powerful and useful in a lot of situations, I find parser generators to be kind of fiddly and onerous to work with for a variety of reasons. On the other hand, writing a parser by hand requires a ton of tedious boilerplate just to get off the ground.

This project is an attempt to find _my_ Goldilocks zone (your mileage may vary) between automagic grammar-based tools and staring into the terrifying abyss of a blank `lib.rs` file. Currently, it provides a _lexer_ generator that's dirt simple to use, some convenience macros, and some barebones parsing primitives inspired by [syn](https://crates.io/crates/syn) &mdash; just enough to give you a rolling start and get out of your way.

It's **extremely** nascent right now, so it's not likely to work for your use case (hell, it probably doesn't even work as intended!), but I think it's a nice, ergonomic foundation to build on.

## Getting Started
Add the dependency to your `Cargo.toml`:

```toml
[dependencies]
gramatika = "0.1"
```

Define an enum for your tokens and derive the `Token` and `Lexer` traits:

```rs
#[macro_use]
extern crate gramatika;

use gramatika::Span;

#[derive(Clone, Copy, PartialEq, Token, Lexer)]
enum Token<'a> {
	#[pattern(r"^(if|else|switch|case|break|for|while|var)\b")]
	Keyword(&'a str, Span),

	#[pattern(r"^(true|false|null)\b")]
	ConstLiteral(&'a str, Span),

	#[pattern(r"^(0[xb])?[0-9A-Fa-f][0-9A-Fa-f.]+")]
	NumLiteral(&'a str, Span),

	#[pattern(r#"^"[^"]+""#)]
	StrLiteral(&'a str, Span),

	#[pattern(r"^[a-zA-Z_][a-zA-Z_0-9]*")]
	Ident(&'a str, Span),

	#[pattern(r"^[(){}[\],.;]")]
	Punct(&'a str, Span),

	#[pattern(r"^[-+*/]")]
	Operator(&'a str, Span),
}
```

`#[derive(Token)]` will implement `gramatika::Token` for your enum, and `#[derive(Lexer)]` will output a `Lexer` struct with the same visibility as the enum.

Next, you'll probably find it useful to declare a type alias for your lexer's `ParseStream`:

```rs
type ParseStream<'a> = gramatika::ParseStream<'a, Token<'a>, Lexer<'a>>;
```

Then define your syntax tree structure:

```rs
struct Program<'a> {
	statements: Vec<Stmt<'a>>,
}

enum Stmt<'a> {
	If(IfStmt<'a>),
	Switch(SwitchStmt<'a>),
	Case(CaseStmt<'a>),
	Break(Token<'a>),
	For(ForStmt<'a>),
	Var(VarStmt<'a>),
	Block(Vec<Stmt<'a>>),
}

// ...
```

Finally, implement `gramatika::Parse` for each of your tree nodes:

```rs
use gramatika::{Parse, ParseStreamer, Result, Span};

// ...

impl<'a> Parse<'a> for Program<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		// Here's where you draw the rest of the f___ing owl... sorry, more useful
		// docs will arrive eventually. In the meantime, feel free to check out
		// the example parser under `examples/lox`.
		todo!()
	}
}
```

With the parser implemented, you can use it to generate your tree:

```rs
fn main<'a>() -> Result<'a, ()> {
	let program: Program = ParseStream::from("var the_answer = 42;").parse()?;
	Ok(())
}
```
