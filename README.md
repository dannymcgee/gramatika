# Gramatika
A minimal toolkit for writing parsers with Rust

## Status
This crate is still pretty young, but starting to feel comfortable in its own skin! It probably wouldn't be wise to give it too much responsibility just yet, but it should be up to the task of handling many common or straightforward use cases. API is subject to change from minor version to minor version until we hit `1.0`, but I'm pretty happy with the general shape of things at this point, so I wouldn't expect any major breaking changes anytime soon.

## Motivation
Though powerful and useful in a lot of situations, I find parser generators to be kind of fiddly and onerous to work with for a variety of reasons. On the other hand, writing a parser by hand requires a ton of tedious boilerplate just to get off the ground.

This project is an attempt to find _my_ Goldilocks zone (your mileage may vary) between automagic grammar-based tools and staring into the terrifying abyss of a blank `lib.rs` file. Currently, it provides a _lexer_ generator that's dirt simple to use, some convenience macros, and some barebones parsing primitives inspired by [syn](https://crates.io/crates/syn) &mdash; just enough to give you a rolling start and get out of your way.

## Getting Started
Add the dependency to your `Cargo.toml`:

```toml
[dependencies]
gramatika = "0.4"
```

Define an enum for your tokens and derive the `Token` and `Lexer` traits:

```rust
#[macro_use]
extern crate gramatika;

use gramatika::{Span, Substr};

#[derive(Clone, PartialEq, Token, Lexer)]
enum Token {
	#[subset_of(Ident)]
	#[pattern = "if|else|switch|case|break|for|while|var"]
	Keyword(Substr, Span),

	#[pattern = "[a-zA-Z_][a-zA-Z_0-9]*"]
	Ident(Substr, Span),

	#[pattern = r"[(){}\[\]]"]
	Brace(Substr, Span),

	#[pattern = "[,.;]"]
	Punct(Substr, Span),

	#[pattern = "[=!<>]=?"]
	#[pattern = "[-+*/]"]
	Operator(Substr, Span),

	#[pattern = "(0[xb])?[0-9A-Fa-f][0-9A-Fa-f.]*"]
	NumLiteral(Substr, Span),

	#[pattern = "\"[^\"]+\""]
	StrLiteral(Substr, Span),
}
```
A few things to note here:

* `#[derive(Token)]` will implement `gramatika::Token` for your enum, and `#[derive(Lexer)]` will output a `Lexer` struct with the same visibility as the enum. `#[derive(Token)]` will also generate a `TokenKind` enum with matching variant names, but no tuple members, which is intended to be used as a friendlier stand-in for `std::mem::discriminant`.
* The `#[pattern = "..."]` attributes define simple regular expressions that your lexer will attempt to match, from top to bottom, anchored to the start of its current cursor position in the source text.
* The `#[subset_of(...)]` attribute here is used to prevent your lexer from greedily matching keywords. Without it, an identifier like `breakdown` would be interpreted as two tokens &mdash; a keyword: `break` followed by an ident: `down`, which is probably not what you wanted. The regex engine doesn't currently support zero-width assertions like `\b`, but I'm hoping to add support for them in the near-ish future.

	An alternative solution would be to put the `Keyword` variant at the very bottom of the enum _without_ a `pattern` annotation, and use `ParseStreamer::upgrade` to convert the relevant `Ident` tokens to `Keyword`s at parse time.
* Spaces, tabs, carriage returns and line feeds are ignored as whitespace by default, but you can override that behavior simply by including them in a pattern.

Next, you'll probably find it useful to declare a type alias for your `ParseStream`:

```rust
type ParseStream = gramatika::ParseStream<Token, Lexer>;
```

Then define your syntax tree structure:

```rust
struct Program {
	statements: Vec<Stmt>,
}

enum Stmt {
	If(IfStmt),
	Switch(SwitchStmt),
	Case(CaseStmt),
	Break(Token),
	For(ForStmt),
	Var(VarStmt),
	Block(Vec<Stmt>),
}

// ...
```

The `gramatika::Parse` trait is the beating heart of your parser. You'll want to implement it for each of your AST nodes:

```rust
use gramatika::{Parse, ParseStreamer, Result, Span, Spanned};

// ...

impl Parse for Program {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let mut stmts = vec![];
		while !input.is_empty() {
			stmts.push(input.parse()?);
		}

		Ok(Self { stmts })
	}
}

impl Parse for Stmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		use TokenKind::*;

		match input.peek() {
			Some(token) => match token.as_matchable() {
				(Keyword, "if", _) => Ok(Stmt::If(input.parse()?)),
				(Keyword, "switch", _) => Ok(Stmt::Switch(input.parse()?)),
				(Keyword, "for", _) => Ok(Stmt::For(input.parse()?)),
				(Keyword, "var", _) => Ok(Stmt::Var(input.parse()?)),
				(Brace, "{", _) => {
					input.consume(brace!["{"])?;

					let mut stmts = vec![];
					while !input.is_empty() && !input.check(brace!["}"]) {
						stmts.push(input.parse()?);
					}

					input.consume(brace!["}"])?;

					Ok(Stmt::Block(stmts))
				}
				(_, _, span) => Err(SpannedError {
					message: "Expected `if`, `switch`, `for`, `var`, or `{`".into(),
					source: input.source(),
					span: Some(span),
				}),
			}
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: input.prev().map(|token| token.span()),
			}),
		}
	}
}

// ...
```
There's a lot going on in that little block of code, but some bullet points to note:

* Calling `as_matchable` on the token isn't strictly necessary, but since the token's lexeme is stored as a reference-counted `Substr` instead of a raw `&str`, it isn't directly `match`able with string-literal patterns. `as_matchable` offers a more ergonomic alternative to match guards for when you need different behavior depending on the specific value of a lexeme.
* That `brace!` macro isn't hard-coded into the library, it's yet another utility that's generated from the `#[derive(...)]` macros on the `Token` enum. It generates one for each of your token variants, and for most of them the quotes are optional &mdash; a natural next step would be to start the `IfStmt::Parse` implementation with a call to `input.consume(keyword![if])?`.
* The `SpannedError` type generates errors with user-friendly formatting when given a reference to the source text and the token span. If we tried to parse an invalid input string here, like `var foo = case "wait, what?": {};`, we would see an error like the following:
	```
	ERROR: Expected expression
	  |
	1 | var foo = case "wait, what?": {};
	  |           ^---
	```
	`Spanned` is a trait, not just a built-in feature of tokens, so you may find it useful to implement it for your AST nodes for use in a later step, like tracking identifier scopes.

After implementing `Parse` for the rest of your nodes, you can initialize your `ParseStream` with some input, and use it to parse your root node:

```rust
fn main() -> Result<()> {
	let program = ParseStream::from("var the_answer = 42;").parse::<Program>()?;
	Ok(())
}
```

This example only scratches the surface, but it should give you an idea of what sort of things the crate is capable of. Check out the docs for `gramatika::parse::ParseStreamer` for all the methods available out-of-the-box on the input stream, and feel free to take a look at the example crates:

* `examples/lox` is a fully working parser for the [Lox programming language](https://craftinginterpreters.com/), making use of all of `gramatika`'s bells and whistles.
* `examples/lox_manual_impl` is a mostly-equivalent copy of the previous `lox` parser, but with `gramatika`'s traits and utilities implemented by hand instead of using the `#[derive(...)]` macros. This is a good place to start digging if you want to see what those macros emit, or if you need to manually implement some (or all) of `gramatika`'s traits for a special use case that the macros don't currently cover.
