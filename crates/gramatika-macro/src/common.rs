use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream, TokenTree};
use quote::format_ident;
use syn::{
	punctuated::Punctuated, token::Comma, Attribute, Fields, GenericParam, Generics,
	LifetimeDef, Variant,
};

pub fn expand_variants(
	variants: &Punctuated<Variant, Comma>,
) -> (Vec<Ident>, Vec<Literal>, Vec<Fields>) {
	variants.iter().cloned().fold(
		(vec![], vec![], vec![]),
		|(mut idents, mut patterns, mut fields), variant| {
			idents.push(variant.ident);
			fields.push(variant.fields);

			if let Some(attr) = variant.attrs.iter().find(|attr| {
				let ident = attr.path.get_ident();
				ident.is_some() && *ident.unwrap() == "pattern"
			}) {
				if let TokenTree::Group(group) =
					attr.tokens.to_owned().into_iter().next().unwrap()
				{
					if let TokenTree::Literal(lit) =
						group.stream().into_iter().next().unwrap()
					{
						patterns.push(lit);
					}
				}
			}

			(idents, patterns, fields)
		},
	)
}

pub fn token_funcs(variant_idents: &[Ident]) -> (Vec<Ident>, Vec<Ident>) {
	variant_idents
		.iter()
		.map(|ident| {
			// FIXME: Need a more robust solution for keyword collisions
			let snake = if *ident == "Type" {
				"ty".into()
			} else {
				format!("{}", ident).to_case(Case::Snake)
			};
			(format_ident!("{}", snake), format_ident!("match_{}", snake))
		})
		.unzip()
}

pub fn lifetime(generics: &Generics) -> &LifetimeDef {
	generics
		.params
		.iter()
		.find_map(|param| match param {
			GenericParam::Lifetime(lifetime) => Some(lifetime),
			_ => None,
		})
		.unwrap()
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
