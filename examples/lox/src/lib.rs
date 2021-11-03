//! This crate contains a simple parser for [Lox](https://craftinginterpreters.com/) as an
//! end-to-end and usability test of the tooling in the `gramatika` crate.

#[macro_use]
extern crate gramatika;

mod decl;
mod expr;
mod stmt;
mod tokens;

use decl::*;
use expr::*;
use stmt::*;
use tokens::*;

pub use tokens::Lexer;

use gramatika::ParseStreamer;

type ParseStream<'a> = gramatika::ParseStream<'a, Token<'a>, Lexer<'a>>;

pub fn parse<'a>(input: String) -> gramatika::Result<'a, Program<'a>> {
	ParseStream::from(input).parse()
}

#[cfg(test)]
mod tests;
