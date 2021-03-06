use itertools::Itertools;
use proc_macro as pm;
use proc_macro2 as pm2;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Field, Fields, Type};

use crate::{
	common::{self, VariantAttrs},
	regex,
};

pub fn derive(input: pm::TokenStream) -> pm::TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	let vis = &ast.vis;
	let ident = &ast.ident;
	let kind_ident = format_ident!("{}Kind", ident);
	let generics = &ast.generics;

	let variants = common::expand_variants(&ast.data);
	let (ctor_ident, _) = common::token_funcs(&variants);
	let variant_match_impl = regex::token_impls(&kind_ident, &variants);

	let (variant_ident, variant_fields): (Vec<_>, Vec<_>) = variants
		.iter()
		.map(|v| (v.ident.clone(), v.fields.clone()))
		.unzip();

	let discarded_kinds = variants
		.iter()
		.enumerate()
		.filter_map(|(idx, var)| {
			let VariantAttrs { discard, .. } = common::extract_variant_attrs(var);
			if discard {
				Some(variant_ident[idx].clone())
			} else {
				None
			}
		})
		.collect_vec();
	let discards_len = discarded_kinds.len();

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
				.map(|field| field.ident.unwrap())
				.collect::<Vec<_>>()
		})
		.collect::<Vec<_>>();

	let result: pm2::TokenStream = quote! {
		#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
		#vis enum #kind_ident {
			#(#variant_ident),*
		}

		macro_rules! hash_set {
			($($elem:path),+ $(,)?) => {{
				let mut set = ::std::collections::HashSet::with_capacity(#discards_len);
				$( set.insert($elem); )+
				set
			}};
			() => {
				::std::collections::HashSet::with_capacity(#discards_len)
			};
		}

		impl #kind_ident {
			fn discards() -> &'static ::std::collections::HashSet<#kind_ident> {
				use ::std::collections::HashSet;
				use ::gramatika::once_cell::sync::OnceCell;

				static DISCARDS: OnceCell<HashSet<#kind_ident>> = OnceCell::new();

				DISCARDS.get_or_init(|| hash_set![
					#( #kind_ident::#discarded_kinds, )*
				])
			}
		}

		impl#generics #ident#generics {
			pub fn as_inner(&self) -> (::gramatika::Substr, ::gramatika::Span) {
				match self {#(
					Self::#variant_ident(lexeme, span) => (lexeme.clone(), *span)
				),*}
			}

			#(pub fn #ctor_ident(#(#ctor_params),*) -> Self {
				Self::#variant_ident(#(#ctor_args),*)
			})*

			#(#variant_match_impl)*
		}

		#(
			#[macro_export]
			macro_rules! #ctor_ident {
				($lexeme:literal) => {
					#ident::#ctor_ident(
						::gramatika::arcstr::literal_substr!($lexeme),
						::gramatika::Span::default(),
					)
				};
				($lexeme:tt) => {
					#ident::#ctor_ident(
						::gramatika::arcstr::literal_substr!(stringify!($lexeme)),
						::gramatika::Span::default(),
					)
				};
			}
		)*

		impl#generics ::gramatika::Token for #ident#generics {
			type Kind = #kind_ident;

			fn lexeme(&self) -> ::gramatika::Substr {
				self.as_inner().0
			}

			fn kind(&self) -> #kind_ident {
				match self {
					#(#ident::#variant_ident(_, _) => #kind_ident::#variant_ident),*
				}
			}

			fn as_matchable(&self) -> (#kind_ident, &str, ::gramatika::Span) {
				match self {#(
					#ident::#variant_ident(lexeme, span) => (
						#kind_ident::#variant_ident,
						lexeme.as_str(),
						*span,
					)
				),*}
			}
		}

		impl#generics ::gramatika::Spanned for #ident#generics {
			fn span(&self) -> ::gramatika::Span {
				self.as_inner().1
			}
		}

		impl#generics Clone for #ident#generics {
			fn clone(&self) -> Self {
				match self {#(
					#ident::#variant_ident(lexeme, span) => #ident::#variant_ident(lexeme.clone(), *span)
				),*}
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
				"Substr" => format_ident!("lexeme"),
				"Span" => format_ident!("span"),
				_ => panic!("Unsupported token field type: `{}`", type_name),
			}
		}
		Type::Reference(ty) => enum_field_type_to_param_name(&ty.elem),
		_ => panic!("Unsupported token field type: `{:?}`", ty),
	}
}
