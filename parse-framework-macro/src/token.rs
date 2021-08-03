use proc_macro as pm;
use proc_macro2 as pm2;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DataEnum, DeriveInput, Field, Fields, Type};

use crate::common;

pub fn derive(input: pm::TokenStream) -> pm::TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	let vis = &ast.vis;
	let ident = &ast.ident;
	let kind_ident = format_ident!("{}Kind", ident);
	let generics = &ast.generics;
	let lifetime = common::lifetime(generics);

	let (variant_ident, variant_pattern, variant_fields) = match &ast.data {
		Data::Enum(DataEnum { variants, .. }) => common::expand_variants(variants),
		_ => unimplemented!(),
	};

	let (ctor_ident, matcher_ident) = common::token_funcs(&variant_ident);
	let ctor_params = variant_fields
		.iter()
		.map(|fields| match fields {
			Fields::Named(fields) => fields.named.iter().cloned().collect::<Vec<_>>(),
			Fields::Unnamed(fields) => fields
				.unnamed
				.iter()
				.cloned()
				.map(|field| Field {
					attrs: field.attrs,
					vis: field.vis,
					ident: Some(enum_field_type_to_param_name(&field.ty)),
					colon_token: field.colon_token,
					ty: field.ty,
				})
				.collect::<Vec<_>>(),
			_ => unreachable!(),
		})
		.collect::<Vec<_>>();

	let ctor_args = ctor_params
		.iter()
		.map(|fields| {
			fields
				.iter()
				.cloned()
				.map(|field| format_ident!("{}", field.ident.unwrap()))
				.collect::<Vec<_>>()
		})
		.collect::<Vec<_>>();

	let result: pm2::TokenStream = quote! {
		#[derive(Clone, Copy, Debug, PartialEq)]
		#vis enum #kind_ident {
			#(#variant_ident),*
		}

		impl#generics #ident#generics {
			fn as_inner(&#lifetime self) -> (&#lifetime str, ::parse_framework::Span) {
				match self {#(
					Self::#variant_ident(lexeme, span) => (*lexeme, *span)
				),*}
			}

			#(pub fn #ctor_ident(#(#ctor_params),*) -> Self {
				Self::#variant_ident(#(#ctor_args),*)
			})*

			#(pub fn #matcher_ident(
				input: &#lifetime str
			) -> ::std::option::Option<::parse_framework::Match<#lifetime>> {
				lazy_static! {
					static ref __VARIANT_PATTERN: ::parse_framework::Regex =
						::parse_framework::Regex::new(#variant_pattern).unwrap();
				}
				__VARIANT_PATTERN.find(input)
			})*
		}

		#(
			#[macro_export]
			macro_rules! #ctor_ident {
				($lexeme:literal) => {
					#ident::#ctor_ident($lexeme, ::parse_framework::Span::default())
				};
				($lexeme:tt) => {
					#ident::#ctor_ident(stringify!($lexeme), ::parse_framework::Span::default())
				};
			}
		)*

		impl#generics ::parse_framework::Token for #ident#generics {
			type Kind = #kind_ident;

			fn lexeme(&self) -> &str {
				self.as_inner().0
			}

			fn span(&self) -> ::parse_framework::Span {
				self.as_inner().1
			}

			fn kind(&self) -> #kind_ident {
				match self {
					#(#ident::#variant_ident(_, _) => #kind_ident::#variant_ident),*
				}
			}
		}

		impl#generics ::parse_framework::Token for &#lifetime #ident#generics {
			type Kind = #kind_ident;

			fn lexeme(&self) -> &str {
				self.as_inner().0
			}

			fn span(&self) -> ::parse_framework::Span {
				self.as_inner().1
			}

			fn kind(&self) -> #kind_ident {
				match self {
					#(#ident::#variant_ident(_, _) => #kind_ident::#variant_ident),*
				}
			}
		}
	};

	result.into()
}

fn enum_field_type_to_param_name(ty: &Type) -> pm2::Ident {
	match ty {
		Type::Path(path) => {
			let type_name = &path.path.segments.iter().last().unwrap().ident;

			match &type_name.to_string()[..] {
				"str" | "String" => format_ident!("lexeme"),
				"Span" => format_ident!("span"),
				_ => panic!("Unsupported token field type: `{:?}`", ty),
			}
		}
		Type::Reference(ty) => enum_field_type_to_param_name(&ty.elem),
		_ => panic!("Unsupported token field type: `{:?}`", ty),
	}
}
