use proc_macro::TokenStream;
use quote::{quote, ToTokens};
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
	let field_method_call = match &data.fields {
		Fields::Named(ref fields) => fields.named.iter().map(|field| {
			let ty = &field.ty;
			let ident = field.ident.as_ref().unwrap();

			if ty.to_token_stream().to_string().starts_with("Option") {
				quote!(.optional_field(stringify!(#ident), self.#ident.as_ref().map(|inner| inner as &dyn ::gramatika::DebugLisp)))
			} else {
				quote!(.field(stringify!(#ident), &self.#ident))
			}
		}),
		Fields::Unnamed(_) => {
			panic!("`#[derive(DebugLisp)]` is not supported for tuple structs")
		}
		Fields::Unit => {
			panic!("`#[derive(DebugLisp)]` is not supported for unit structs")
		}
	};

	let stream = quote! {
		impl#generics ::gramatika::DebugLisp for #ident#generics {
			fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>, indent: usize) -> ::core::fmt::Result {
				::gramatika::DebugLispStruct::new(f, indent, stringify!(#ident))
					#(#field_method_call)*
					.finish()
			}
		}

		impl#generics ::core::fmt::Debug for #ident#generics {
			fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
				::gramatika::DebugLisp::fmt(self, f, 0)
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
		impl#generics ::gramatika::DebugLisp for #ident#generics {
			fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
				write!(f, "({}::", stringify!(#ident))?;

				match self {#(
					#ident::#variant_name(inner) => {
						write!(f, "{} ", stringify!(#variant_name))?;
						<#variant_inner_type as ::gramatika::DebugLisp>::fmt(&inner, f, indent)
					}
				)*}?;

				write!(f, ")")
			}
		}

		impl#generics ::core::fmt::Debug for #ident#generics {
			fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
				::gramatika::DebugLisp::fmt(self, f, 0)
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
		impl#generics ::gramatika::DebugLisp for #ident#generics {
			fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, _: usize) -> ::std::fmt::Result {
				write!(
					f,
					"`{}` ({:?} ({:?}))",
					<Self as ::gramatika::Token>::lexeme(self),
					<Self as ::gramatika::Token>::kind(self),
					<Self as ::gramatika::Spanned>::span(self)
				)
			}
		}
	};

	stream.into()
}
