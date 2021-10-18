//! NOTE: This crate only exists to debug macros.
//! Run `cargo expand -p expand` to check the output of the macros used here.

use gramatika::Span;

#[macro_use]
extern crate gramatika;

#[allow(dead_code)]
#[derive(Clone, Copy, DebugLispToken, PartialEq, Token, Lexer)]
pub enum Token<'a> {
	#[pattern = r"\[\[?|\]\]?|[(){}]"]
	Brace(&'a str, Span),

	#[pattern = r"/{2}.*"]
	Comment(&'a str, Span),

	#[pattern = r"(array|atomic|bool|[fiu]32|mat[2-4]x[2-4]|ptr|sampler(_comparison)?|vec[2-4])\b"]
	#[pattern = r"(texture_multisampled_2d)\b"]
	#[pattern = r"(texture_external)\b"]
	#[pattern = r"(texture_depth_(2d|cube)(_array)?)\b"]
	#[pattern = r"(texture_(1d|2d(_array)?|3d|cube(_array)?))\b"]
	#[pattern = r"(texture_storage_(1d|2d(_array)?|3d))\b"]
	Type(&'a str, Span),

	#[pattern = r"(fn|let|struct|type|var|export)\b"]
	#[pattern = r"(function|private|read(_write)?|storage|uniform|workgroup|write)\b"]
	#[pattern = r"(break|case|continu(e|ing)|default|else(if)?|fallthrough|for|if|loop|return|switch|from)\b"]
	#[pattern = r"(true|false)\b"]
	#[pattern = r"(bitcast|discard|enable|import)\b"]
	Keyword(&'a str, Span),

	#[pattern = "[a-zA-Z][0-9a-zA-Z_]*"]
	Ident(&'a str, Span),

	#[pattern = "->"]
	#[pattern = r"&&?|\|\|?|--?|\+\+?|>>|<<"]
	#[pattern = "[=!<>]=?"]
	#[pattern = "[%*/~^]"]
	Operator(&'a str, Span),

	#[pattern = r"::?|[,.;]"]
	Punct(&'a str, Span),

	#[pattern = r"-?[0-9]*\.[0-9]+"]
	#[pattern = r"-?[0-9]+\.[0-9]*"]
	#[pattern = "[eE][-+]?[0-9]+"]
	#[pattern = r"-?0x[0-9a-fA-F]*\.?[0-9a-fA-F]+"]
	#[pattern = r"-?0x[0-9a-fA-F]+\.?[0-9a-fA-F]*"]
	#[pattern = "[pP][-+]?[0-9]+"]
	Literal(&'a str, Span),

	NoPattern1(&'a str, Span),
	NoPattern2(&'a str, Span),
	NoPattern3(&'a str, Span),
}

fn main() {}
