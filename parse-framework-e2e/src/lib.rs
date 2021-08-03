//! This crate contains a simple parser for [Lox](https://craftinginterpreters.com/) as an
//! end-to-end and usability test of the tooling in the `parse_framework` crate.

#![allow(unused_imports)]

#[macro_use]
extern crate parse_framework;

mod decl;
mod expr;
mod stmt;
mod tokens;

use decl::*;
use expr::*;
use stmt::*;
use tokens::*;

#[cfg(test)]
mod tests;
