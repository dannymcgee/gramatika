use proc_macro::TokenStream;
use quote::quote;
use syn::{
	parse_macro_input, Data, DataEnum, DataStruct, DeriveInput, Fields, Generics, Ident,
};

pub fn derive(input: TokenStream) -> TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	let ident = &ast.ident;
	let generics = &ast.generics;

	match &ast.data {
		Data::Struct(data) => derive_debug_struct(ident, generics, data),
		Data::Enum(data) => derive_debug_enum(ident, generics, data),
		Data::Union(_) => {
			panic!("`#[derive(DebugLisp)]` is not supported for Union types")
		}
	}
}

fn derive_debug_struct(
	ident: &Ident,
	generics: &Generics,
	data: &DataStruct,
) -> TokenStream {
	let field_name = match &data.fields {
		Fields::Named(ref fields) => fields
			.named
			.iter()
			.map(|field| field.ident.as_ref().unwrap()),
		Fields::Unnamed(_) => {
			panic!("`#[derive(DebugLisp)]` is not supported for tuple structs")
		}
		Fields::Unit => {
			panic!("`#[derive(DebugLisp)]` is not supported for unit structs")
		}
	};

	let stream = quote! {
		impl#generics ::parse_framework::DebugLisp for #ident#generics {
			fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>, indent: usize) -> ::core::fmt::Result {
				::parse_framework::DebugLispStruct::new(f, indent, stringify!(#ident))
					#(.field(stringify!(#field_name), &self.#field_name))*
					.finish()
			}
		}

		impl#generics ::core::fmt::Debug for #ident#generics {
			fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
				::parse_framework::DebugLisp::fmt(self, f, 0)
			}
		}
	};

	stream.into()
}

fn derive_debug_enum(ident: &Ident, generics: &Generics, data: &DataEnum) -> TokenStream {
	let variant_name = data.variants.iter().map(|variant| &variant.ident);
	let variant_inner_type = data
		.variants
		.iter()
		.map(|variant| match &variant.fields {
			Fields::Unnamed(fields) => fields.unnamed.iter().map(|field| &field.ty),
			Fields::Named(_) => {
				panic!("`#[derive(DebugLisp)]` is not supported for enum variants with named fields")
			}
			Fields::Unit => {
				panic!("`#[derive(DebugLisp)]` is not supported for unit enum variants")
			}
		})
		.flatten();

	let stream = quote! {
		impl#generics ::parse_framework::DebugLisp for #ident#generics {
			fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
				write!(f, "({}::", stringify!(#ident))?;

				match self {#(
					#ident::#variant_name(inner) => {
						write!(f, "{} ", stringify!(#variant_name))?;
						<#variant_inner_type as ::parse_framework::DebugLisp>::fmt(&inner, f, indent)
					}
				)*}?;

				write!(f, ")")
			}
		}
	};

	stream.into()
}

pub fn derive_token(input: TokenStream) -> TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	let ident = &ast.ident;
	let generics = &ast.generics;

	let stream = quote! {
		impl#generics ::parse_framework::DebugLisp for #ident#generics {
			fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, _: usize) -> ::std::fmt::Result {
				write!(
					f,
					"`{}` ({:?} ({:?}))",
					<Self as ::parse_framework::Token>::lexeme(self),
					<Self as ::parse_framework::Token>::kind(self),
					<Self as ::parse_framework::Token>::span(self)
				)
			}
		}
	};

	stream.into()
}
