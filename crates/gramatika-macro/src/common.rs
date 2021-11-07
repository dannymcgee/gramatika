use convert_case::{Case, Casing};
use itertools::Itertools;
use proc_macro2::{self as pm2, Ident, Literal, TokenTree};
use quote::{format_ident, quote};
use syn::{punctuated::Punctuated, token::Comma, Fields, Variant};

use crate::{common, patterns};

#[derive(Debug, Default)]
pub struct TokenMeta {
	pub idents: Vec<Ident>,
	pub pattern_bodies: Vec<pm2::TokenStream>,
	pub match_bodies: Vec<pm2::TokenStream>,
	pub fields: Vec<Fields>,
}

pub fn expand_variants(variants: &Punctuated<Variant, Comma>) -> TokenMeta {
	variants
		.iter()
		.cloned()
		.fold(TokenMeta::default(), |mut meta, variant| {
			let patterns = extract_patterns(&variant);
			let regex_init = if !patterns.is_empty() {
				match build_regex_init(patterns) {
					Ok(regex) => Some(regex),
					Err(err) => panic!("{}", err),
				}
			} else {
				None
			};

			if let Some(regex_init) = regex_init {
				let screaming = variant.ident.to_string().to_case(Case::ScreamingSnake);
				#[allow(non_snake_case)]
				let PATTERN_IDENT = format_ident!("{}_PATTERN", screaming);

				let snake = variant.ident.to_string().to_case(Case::Snake);
				let get_pattern = format_ident!("{}_pattern", snake);

				let pattern_body = quote! {
					use ::gramatika::{
						once_cell::sync::OnceCell,
						regex_automata::{SparseDFA, Regex},
					};
					use ::std::sync::RwLock;

					static #PATTERN_IDENT: OnceCell<RwLock<Regex<SparseDFA<&'static [u8], u32>>>>
						= OnceCell::new();

					#PATTERN_IDENT.get_or_init(|| RwLock::new(#regex_init))
				};

				let match_body = match find_superset_matcher(&variant) {
					Some(match_superset) => {
						quote! {
							Self::#match_superset(input).and_then(|(start, end)| {
								match Self::#get_pattern()
									.read()
									.unwrap()
									.find(input.as_bytes()) {
										Some((s, e)) if s == start && e == end => {
											Some((start, end))
										}
										_ => None,
									}
							})
						}
					}
					None => {
						quote! {
							Self::#get_pattern()
								.read()
								.unwrap()
								.find(input.as_bytes())
						}
					}
				};

				meta.pattern_bodies.push(pattern_body);
				meta.match_bodies.push(match_body);
			}

			meta.idents.push(variant.ident);
			meta.fields.push(variant.fields);

			meta
		})
}

pub fn regex_literal(lit: &Literal) -> String {
	let source = lit.to_string();

	let mut front_offset = 0;
	let mut back_offset = 0;

	for c in source.chars() {
		match c {
			'r' => {
				front_offset += 1;
			}
			'#' => {
				front_offset += 1;
				back_offset += 1;
			}
			'"' => {
				front_offset += 1;
				back_offset += 1;
				break;
			}
			_ => break,
		}
	}

	let inner = &source[front_offset..source.len() - back_offset];

	inner.into()
}

pub fn token_funcs(variant_idents: &[Ident]) -> (Vec<Ident>, Vec<Ident>, Vec<Ident>) {
	let snakes = variant_idents
		.iter()
		.map(|ident| {
			// FIXME: Need a more robust solution for keyword collisions
			match &ident.to_string()[..] {
				"Type" => "ty".into(),
				"Struct" => "structure".into(),
				other => other.to_string().to_case(Case::Snake),
			}
		})
		.collect::<Vec<_>>();

	snakes
		.iter()
		.map(|snake| {
			(
				format_ident!("{}", snake),
				format_ident!("match_{}", snake),
				format_ident!("{}_pattern", snake),
			)
		})
		.multiunzip()
}

fn extract_patterns(variant: &syn::Variant) -> Vec<Literal> {
	variant
		.attrs
		.iter()
		.filter_map(|attr| {
			let ident = attr.path.get_ident();
			if ident.is_some() && *ident.unwrap() == "pattern" {
				attr.tokens.clone().into_iter().find_map(|tt| match tt {
					TokenTree::Literal(lit) => Some(lit),
					_ => None,
				})
			} else {
				None
			}
		})
		.collect()
}

fn build_regex_init(patterns: Vec<Literal>) -> anyhow::Result<pm2::TokenStream> {
	if patterns.len() == 1 {
		let lit = patterns.into_iter().last().unwrap();
		let pattern = common::regex_literal(&lit);

		patterns::compile(&pattern)
	} else {
		let combined = patterns
			.iter()
			.map(|lit| {
				let inner = common::regex_literal(lit);
				format!("({})", inner)
			})
			.join("|");

		patterns::compile(&combined)
	}
}

fn find_superset_matcher(variant: &syn::Variant) -> Option<Ident> {
	variant.attrs.iter().find_map(|attr| {
		let ident = attr.path.get_ident();
		if ident.is_some() && *ident.unwrap() == "subset_of" {
			attr.tokens.clone().into_iter().find_map(|tt| match tt {
				TokenTree::Group(g) => match g.stream().into_iter().last() {
					Some(TokenTree::Ident(ident)) => {
						let snake = ident.to_string().to_case(Case::Snake);
						Some(format_ident!("match_{}", snake))
					}
					_ => None,
				},
				_ => None,
			})
		} else {
			None
		}
	})
}
