//! This is a copy of the other Lox example, but with manual implementations of all the
//! [`gramatika`] traits. It serves a few purposes.
//!
//! ### For contributors:
//!
//! 1. Proc macros are kind of a pain to author. Having a fully manual implementation of
//!    gramatika's whole feature-set makes it much easier, since we can test, debug, and
//!    iterate on new ideas in plain Rust code, and then copy those implementations into
//!    macro-land once they're ready.
//! 2. Along the same lines, while new features are in development here, the derived Lox
//!    example can act as a baseline to benchmark against, to make sure the happy path
//!    stays fast.
//!
//! ### For users:
//!
//! 1. It offers a lot of insight into what gramatika is actually doing "under the hood",
//!    which can be hard to glean from the macro implementations.
//! 2. It can act as a sort of guide for implementing the gramatika traits manually for
//!    users who are averse to macros or have an out-of-the-box use case that the macros
//!    don't currently cover.

#[macro_use]
extern crate gramatika;

mod decl;
mod expr;
mod lexer;
mod parse;
mod stmt;
mod tokens;

use gramatika::ParseStreamer;
use parse::ParseStream;
use stmt::Program;

pub use lexer::Lexer;

pub fn parse(input: String) -> gramatika::Result<Program> {
	ParseStream::from(input).parse()
}

#[cfg(test)]
mod tests;
