//! This crate contains a simple parser for [Lox](https://craftinginterpreters.com/) as an
//! end-to-end and usability test of the tooling in the `gramatika` crate.

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
