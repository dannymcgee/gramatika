use convert_case::{Case, Casing};
use proc_macro2::{self as pm2, Ident};
use quote::format_ident;
use syn::{punctuated::Punctuated, token::Comma, Fields, Variant};

use crate::regex;

#[derive(Debug, Default)]
pub struct TokenMeta {
	pub idents: Vec<Ident>,
	pub regex_match_impls: Vec<pm2::TokenStream>,
	pub fields: Vec<Fields>,
}

pub struct VariantIdents {
	pub ctor: Ident,
	pub pattern: Ident,
	pub get_pattern: Ident,
	pub match_: Ident,
}

impl VariantIdents {
	pub fn new(variant: &Ident) -> Self {
		let ident = variant.to_string();
		let snake = ident.to_case(Case::Snake);
		let screaming = ident.to_case(Case::ScreamingSnake);

		let ctor =
			if matches!(
				snake.as_str(),
				"as" | "auto"
					| "box" | "break" | "self"
					| "catch" | "const" | "continue"
					| "crate" | "default"
					| "do" | "dyn" | "else"
					| "enum" | "extern" | "fn"
					| "for" | "if" | "impl"
					| "in" | "let" | "loop"
					| "macro" | "match" | "mod"
					| "move" | "mut" | "pub"
					| "ref" | "return" | "static"
					| "struct" | "super" | "trait"
					| "type" | "union" | "unsafe"
					| "use" | "where" | "while"
					| "yield"
			) {
				format_ident!("{}_", snake)
			} else {
				format_ident!("{}", snake)
			};

		let pattern = format_ident!("{}_PATTERN", screaming);
		let get_pattern = format_ident!("{}_pattern", snake);
		let match_ = format_ident!("match_{}", snake);

		Self {
			ctor,
			pattern,
			get_pattern,
			match_,
		}
	}
}

pub fn expand_variants(variants: &Punctuated<Variant, Comma>) -> TokenMeta {
	variants
		.iter()
		.cloned()
		.fold(TokenMeta::default(), |mut meta, variant| {
			meta.regex_match_impls.push(regex::impls(&variant));
			meta.idents.push(variant.ident);
			meta.fields.push(variant.fields);

			meta
		})
}

pub fn token_funcs(variant_idents: &[Ident]) -> (Vec<Ident>, Vec<Ident>) {
	variant_idents
		.iter()
		.map(|ident| {
			let VariantIdents { ctor, match_, .. } = VariantIdents::new(ident);
			(ctor, match_)
		})
		.unzip()
}
