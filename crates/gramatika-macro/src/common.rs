use convert_case::{Case, Casing};
use proc_macro2::Ident;
use quote::format_ident;
use syn::{Data, DataEnum, Variant};

use crate::regex;

pub struct VariantIdents {
	pub variant: Ident,
	pub ctor: Ident,
	pub pattern: Ident,
	pub get_pattern: Ident,
	pub match_: Option<Ident>,
}

impl VariantIdents {
	pub fn new(variant: &Variant) -> Self {
		let ident = variant.ident.to_string();
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

		let (subset, patterns) = regex::extract_variant_attrs(variant);
		let match_ = if !patterns.is_empty() && subset.is_none() {
			Some(format_ident!("match_{}", snake))
		} else {
			None
		};

		Self {
			variant: variant.ident.clone(),
			ctor,
			pattern,
			get_pattern,
			match_,
		}
	}
}

pub fn expand_variants(data: &Data) -> Vec<Variant> {
	match data {
		Data::Enum(DataEnum { variants, .. }) => variants.iter().cloned().collect(),
		_ => unimplemented!(),
	}
}

pub fn token_funcs(variants: &[Variant]) -> (Vec<Ident>, Vec<Ident>) {
	let (ctors, matchers): (Vec<_>, Vec<_>) = variants
		.iter()
		.map(|variant| {
			let VariantIdents { ctor, match_, .. } = VariantIdents::new(variant);
			(ctor, match_)
		})
		.unzip();

	let matchers = matchers.into_iter().flatten().collect();

	(ctors, matchers)
}
