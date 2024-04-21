use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenTree};
use quote::format_ident;
use syn::{Data, DataEnum, Variant};

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
					| "move" | "mut" | "path"
					| "pub" | "ref" | "return"
					| "static" | "struct"
					| "super" | "trait" | "type"
					| "union" | "unsafe" | "use"
					| "where" | "while" | "yield"
			) {
				format_ident!("{}_", snake)
			} else {
				format_ident!("{}", snake)
			};

		let pattern = format_ident!("{}_PATTERN", screaming);
		let get_pattern = format_ident!("{}_pattern", snake);

		let VariantAttrs {
			subset, patterns, ..
		} = extract_variant_attrs(variant);

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

pub struct VariantAttrs {
	pub subset: Option<Ident>,
	pub patterns: Vec<Literal>,
	pub discard: bool,
}

pub fn extract_variant_attrs(variant: &Variant) -> VariantAttrs {
	let mut subset = None;
	let mut patterns = vec![];
	let mut discard = false;

	for attr in variant.attrs.iter() {
		match attr.path.get_ident() {
			Some(ident) if ident == "pattern" => {
				let lit = attr.tokens.clone().into_iter().find_map(|tt| match tt {
					TokenTree::Literal(lit) => Some(lit),
					_ => None,
				});
				if let Some(lit) = lit {
					patterns.push(lit)
				}
			}
			Some(ident) if ident == "subset_of" => {
				subset = attr.tokens.clone().into_iter().find_map(|tt| match tt {
					TokenTree::Group(g) => match g.stream().into_iter().last() {
						Some(TokenTree::Ident(ident)) => Some(ident),
						_ => None,
					},
					_ => None,
				});
			}
			Some(ident) if ident == "discard" => {
				discard = true;
			}
			_ => {}
		}
	}

	VariantAttrs {
		subset,
		patterns,
		discard,
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
