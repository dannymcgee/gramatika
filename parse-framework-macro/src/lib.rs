use convert_case::{Case, Casing};
use proc_macro as pm;
use proc_macro2 as pm2;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DataEnum, DeriveInput, Field, Fields};

#[proc_macro_derive(Token)]
pub fn derive(input: pm::TokenStream) -> pm::TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	// eprintln!("{:#?}", ast);

	let ident = &ast.ident;
	let generics = &ast.generics;

	let (variant_ident, variant_fields) = match &ast.data {
		Data::Enum(DataEnum { variants, .. }) => variants.iter().cloned().fold(
			(vec![], vec![]),
			|(mut idents, mut fields), variant| {
				idents.push(variant.ident);
				fields.push(variant.fields);

				(idents, fields)
			},
		),
		_ => unimplemented!(),
	};

	let func_ident = variant_ident.iter().map(|ident| {
		let snake = format!("{}", ident).to_case(Case::Snake);
		format_ident!("{}", snake)
	});

	let func_params = variant_fields
		.iter()
		.map(|fields| match fields {
			Fields::Named(fields) => fields.named.iter().cloned().collect::<Vec<_>>(),
			Fields::Unnamed(fields) => fields
				.unnamed
				.iter()
				.cloned()
				.enumerate()
				.map(|(idx, field)| Field {
					attrs: field.attrs,
					vis: field.vis,
					ident: Some(format_ident!("arg{}", idx)),
					colon_token: field.colon_token,
					ty: field.ty,
				})
				.collect::<Vec<_>>(),
			_ => unreachable!(),
		})
		.collect::<Vec<_>>();

	let func_param_names = func_params
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
		impl#generics #ident#generics {
			#(
				pub fn #func_ident(#(#func_params),*) -> Self {
					Self::#variant_ident(#(#func_param_names),*)
				}
			)*
		}
	};

	result.into()
}
