use std::collections::HashMap;

use itertools::Itertools;
use pm2::{Group, Ident, Literal, TokenTree};
use proc_macro as pm;
use proc_macro2 as pm2;
use quote::quote;
use regex_automata::RegexBuilder;
use syn::{Attribute, Variant};

use crate::common::VariantIdents;

pub fn proc(input: pm::TokenStream) -> pm::TokenStream {
	let pattern = pm2::TokenStream::from(input)
		.into_iter()
		.filter_map(|tt| match tt {
			TokenTree::Literal(lit) => {
				let inner = from_literal(&lit);
				Some(format!("({})", inner))
			}
			_ => None,
		})
		.join("|");

	compile(&pattern).unwrap().into()
}

pub fn token_impls(kind_ident: &Ident, variants: &[Variant]) -> Vec<pm2::TokenStream> {
	let subset_map = find_subset_matchers(variants);
	variants
		.iter()
		.map(|v| token_impl(kind_ident, v, &subset_map))
		.collect()
}

fn token_impl(
	kind_ident: &Ident,
	variant: &Variant,
	subset_matchers: &HashMap<Variant, Vec<Variant>>,
) -> pm2::TokenStream {
	let (_, patterns) = extract_variant_attrs(variant);
	if patterns.is_empty() {
		return quote! {};
	}

	let init = match init_expr(patterns) {
		Ok(regex) => Some(regex),
		Err(err) => panic!("{}", err),
	}
	.unwrap();

	let idents = VariantIdents::new(variant);
	let get_pattern_impl = get_pattern_impl(&idents, init);
	let match_impl_body =
		match_impl_body(kind_ident, &idents, subset_matchers.get(variant));

	let match_impl = match idents.match_ {
		Some(match_) => {
			quote! {
				pub fn #match_(input: &str) -> ::std::option::Option<(usize, usize, #kind_ident)> {
					#match_impl_body
				}
			}
		}
		None => {
			quote! {}
		}
	};

	quote! {
		#get_pattern_impl

		#match_impl
	}
}

fn get_pattern_impl(idents: &VariantIdents, init: pm2::TokenStream) -> pm2::TokenStream {
	let pattern = &idents.pattern;
	let get_pattern = &idents.get_pattern;
	quote! {
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
	}
}

fn match_impl_body(
	kind_ident: &Ident,
	idents: &VariantIdents,
	subset_matchers: Option<&Vec<Variant>>,
) -> pm2::TokenStream {
	let variant_ident = &idents.variant;
	let get_pattern = &idents.get_pattern;

	if let Some(subset_matchers) = subset_matchers {
		let result_expr = quote! {
			Some((start, end, #kind_ident::#variant_ident))
		};

		let match_subsets =
			subset_matchers
				.iter()
				.rev()
				.fold(result_expr, |accum, variant| {
					let variant_ident = &variant.ident;
					let VariantIdents { get_pattern, .. } = VariantIdents::new(variant);
					quote! {
						match Self::#get_pattern()
							.read()
							.unwrap()
							.find(input.as_bytes())
						{
							Some((s, e)) if s == start && e == end => {
								Some((start, end, #kind_ident::#variant_ident))
							}
							_ => #accum
						}
					}
				});

		quote! {
			match Self::#get_pattern()
				.read()
				.unwrap()
				.find(input.as_bytes())
			{
				Some((start, end)) => #match_subsets
				None => None,
			}
		}
	} else {
		quote! {
			Self::#get_pattern()
				.read()
				.unwrap()
				.find(input.as_bytes())
				.map(|(start, end)| (start, end, #kind_ident::#variant_ident))
		}
	}
}

pub fn extract_variant_attrs(variant: &Variant) -> (Option<Ident>, Vec<Literal>) {
	let mut subset = None;
	let mut patterns = vec![];

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
			_ => {}
		}
	}

	(subset, patterns)
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

		Regex::<SparseDFA<&[u8], u32>>::from_dfas(fwd, rev)
	}};

	Ok(regex)
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

fn find_subset_matchers(variants: &[Variant]) -> HashMap<Variant, Vec<Variant>> {
	variants
		.iter()
		.filter_map(|variant| {
			let subsetters = variants
				.iter()
				.filter_map(|v| {
					let extract_subset_variant =
						|group: Group| match group.stream().into_iter().last() {
							Some(TokenTree::Ident(ident)) if ident == variant.ident => {
								Some(v.clone())
							}
							_ => None,
						};

					let find_subset_attr = |attr: &Attribute| match attr.path.get_ident()
					{
						Some(ident) if ident == "subset_of" => {
							attr.tokens.clone().into_iter().find_map(|tt| match tt {
								TokenTree::Group(g) => extract_subset_variant(g),
								_ => None,
							})
						}
						_ => None,
					};

					v.attrs.iter().find_map(find_subset_attr)
				})
				.collect::<Vec<_>>();

			if !subsetters.is_empty() {
				Some((variant.clone(), subsetters))
			} else {
				None
			}
		})
		.collect()
}
