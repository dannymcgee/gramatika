use convert_case::{Case, Casing};
use itertools::Itertools;
use proc_macro2::{Ident, Literal, TokenStream, TokenTree};
use quote::format_ident;
use syn::{punctuated::Punctuated, token::Comma, Attribute, Fields, Variant};

use crate::common;

pub fn expand_variants(
	variants: &Punctuated<Variant, Comma>,
) -> (Vec<Ident>, Vec<Literal>, Vec<Fields>) {
	variants.iter().cloned().fold(
		(vec![], vec![], vec![]),
		|(mut idents, mut patterns, mut fields), variant| {
			idents.push(variant.ident);
			fields.push(variant.fields);

			let pat_literals = variant
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
				.collect::<Vec<_>>();

			if pat_literals.len() == 1 {
				patterns.push(transform_regex(pat_literals.into_iter().last().unwrap()));
			} else if !pat_literals.is_empty() {
				let combined = pat_literals
					.iter()
					.map(|lit| {
						let inner = common::regex_literal(lit);
						format!("^({})", inner)
					})
					.join("|");
				let pattern =
					syn::parse_str::<Literal>(&format!("r#\"{}\"#", combined)).unwrap();

				patterns.push(pattern);
			}

			(idents, patterns, fields)
		},
	)
}

fn transform_regex(lit: Literal) -> Literal {
	let inner = regex_literal(&lit);
	if inner.starts_with('^') {
		lit
	} else {
		let pattern = format!("r#\"^({})\"#", inner);
		syn::parse_str::<Literal>(&pattern).unwrap()
	}
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

pub fn token_funcs(
	variant_idents: &[Ident],
	variant_patterns: &[Literal],
) -> (Vec<Ident>, Vec<Ident>) {
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

	let ctors = snakes.iter().map(|snake| format_ident!("{}", snake));
	let matchers = variant_patterns.iter().enumerate().map(|(idx, _)| {
		let snake = &snakes[idx];
		format_ident!("match_{}", snake)
	});

	(ctors.collect(), matchers.collect())
}

pub fn attr_args(attr: &Attribute) -> Option<TokenStream> {
	attr.tokens
		.to_owned()
		.into_iter()
		.next()
		.and_then(|tt| match tt {
			TokenTree::Group(group) => Some(group.stream()),
			_ => None,
		})
}
