use convert_case::{Case, Casing};
use itertools::Itertools;
use pm2::{Ident, Literal, TokenTree};
use proc_macro2 as pm2;
use quote::{format_ident, quote};
use regex_automata::RegexBuilder;
use syn::Variant;

use crate::common::VariantIdents;

pub fn impls(variant: &Variant) -> pm2::TokenStream {
	let patterns = extract_pattern_attrs(variant);
	if !patterns.is_empty() {
		let init = match init_expr(patterns) {
			Ok(regex) => Some(regex),
			Err(err) => panic!("{}", err),
		};

		let VariantIdents {
			pattern,
			get_pattern,
			match_,
			..
		} = VariantIdents::new(&variant.ident);

		let get_pattern_impl = quote! {
			fn #get_pattern()
			-> &'static ::std::sync::RwLock<
				::gramatika::regex_automata::Regex<
					::gramatika::regex_automata::SparseDFA<
						&'static [u8], u32>>>
			{
				use ::gramatika::{
					once_cell::sync::OnceCell,
					regex_automata::{SparseDFA, Regex},
				};
				use ::std::sync::RwLock;

				static #pattern: OnceCell<RwLock<Regex<SparseDFA<&'static [u8], u32>>>> = OnceCell::new();

				#pattern.get_or_init(|| RwLock::new(#init))
			}
		};

		let match_impl_body = match find_superset_matcher(variant) {
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

		quote! {
			#get_pattern_impl

			pub fn #match_(input: &str) -> ::std::option::Option<(usize, usize)> {
				#match_impl_body
			}
		}
	} else {
		quote! {}
	}
}

fn compile(pattern: &str) -> anyhow::Result<pm2::TokenStream> {
	let re = RegexBuilder::new().anchored(true).build_sparse(pattern)?;

	let fwd = re.forward().to_u32()?.to_bytes_native_endian()?;
	let fwd = fwd
		.iter()
		.map(|b| pm2::Literal::u8_unsuffixed(*b))
		.collect_vec();
	let fwd = quote! { &[#(#fwd),*] };

	let rev = re.reverse().to_u32()?.to_bytes_native_endian()?;
	let rev = rev
		.iter()
		.map(|b| pm2::Literal::u8_unsuffixed(*b))
		.collect_vec();
	let rev = quote! { &[#(#rev),*] };

	let regex = quote! {{
		let fwd = unsafe { SparseDFA::from_bytes(#fwd) };
		let rev = unsafe { SparseDFA::from_bytes(#rev) };

		Regex::from_dfas(fwd, rev)
	}};

	Ok(regex)
}

fn extract_pattern_attrs(variant: &Variant) -> Vec<Literal> {
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

fn init_expr(patterns: Vec<Literal>) -> anyhow::Result<pm2::TokenStream> {
	if patterns.len() == 1 {
		let lit = patterns.into_iter().last().unwrap();
		let pattern = from_literal(&lit);

		compile(&pattern)
	} else {
		let combined = patterns
			.iter()
			.map(|lit| {
				let inner = from_literal(lit);
				format!("({})", inner)
			})
			.join("|");

		compile(&combined)
	}
}

fn from_literal(lit: &Literal) -> String {
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

fn find_superset_matcher(variant: &Variant) -> Option<Ident> {
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
