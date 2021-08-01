use convert_case::{Case, Casing};
use proc_macro as pm;
use proc_macro2 as pm2;
use quote::{format_ident, quote};
use syn::{
	parse_macro_input, Data, DataEnum, DeriveInput, Field, Fields, GenericParam, Type,
};

#[proc_macro_derive(Token)]
pub fn derive(input: pm::TokenStream) -> pm::TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	let ident = &ast.ident;
	let generics = &ast.generics;

	let lifetime = generics
		.params
		.iter()
		.find_map(|param| match param {
			GenericParam::Lifetime(lifetime) => Some(lifetime),
			_ => None,
		})
		.unwrap();

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
			fn as_inner(&#lifetime self) -> (&#lifetime str, Span) {
				match self {#(
					Self::#variant_ident(lexeme, span) => (*lexeme, *span)
				),*}
			}

			#(
				pub fn #func_ident(#(#func_params),*) -> Self {
					Self::#variant_ident(#(#func_param_names),*)
				}
			)*
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
