use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

use crate::common;

pub fn derive(input: TokenStream) -> TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	let ident = &ast.ident;
	let generics = &ast.generics;

	let token_type =
		ast.attrs
			.iter()
			.find_map(|attr| {
				attr.path.get_ident().and_then(|ident| {
					if *ident == "parse_token" {
						Some(common::attr_args(attr).expect(
							"`#[parse_token(...)` attribute has required arguments",
						))
					} else {
						None
					}
				})
			})
			.expect("`#[parse_token(...)` attribute must be added to the struct");

	let (field_name, method_call): (Vec<_>, Vec<_>) = match &ast.data {
		syn::Data::Struct(inner) => match inner.fields {
			syn::Fields::Named(ref fields) => fields.named.iter().filter_map(|field| {
				let field_name = field.ident.as_ref().unwrap();
				let method_call = field.attrs.iter().find_map(|attr| {
					attr.path
						.get_ident()
						.and_then(|ident| match &ident.to_string()[..] {
							"token" => {
								let attr_value = common::attr_args(attr).expect(
									"`#[token(...)]` attribute has required arguments",
								);
								Some(quote! { take(#attr_value) })
							}
							"token_kind" => {
								let attr_value = common::attr_args(attr).expect(
									"`#[token_kind(...)]` attribute has required arguments",
								);
								Some(quote! {
									take_kind(TokenKind::#attr_value)
								})
							}
							_ => None,
						})
				});

				method_call.map(|method_call| (field_name, method_call))
			}),
			_ => unimplemented!(),
		},
		_ => unimplemented!(),
	}
	.unzip();

	let result = quote! {
		impl#generics ::gramatika::Parse for #ident#generics {
			type Token = #token_type;

			fn parse(
				input: &mut ::gramatika::ParseStream<Self::Token>
			) -> std::result::Result<Self, ::std::string::String> {
				Ok(Self {
					#(
						#field_name: input.#method_call?
					),*
				})
			}
		}
	};

	result.into()
}
