use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{
	parse_macro_input,
	token::{And, Dyn, Mut, SelfValue},
};

mod codegen;
mod parse;

pub fn visitor(input: TokenStream) -> TokenStream {
	let def = parse_macro_input!(input as VisitorDef);
	let output = quote!(#def);

	output.into()
}

struct VisitorDef {
	visitor_ident: Ident,
	walker_ident: Option<Ident>,
	receiver: Annotated<SelfValue>,
	signatures: Vec<Signature>,
}

struct Signature {
	receiver: Annotated<SelfValue>,
	param_type: Annotated<Ident>,
	ret: Option<Annotated<Ident>>,
}

#[derive(Clone)]
struct Annotated<T> {
	ident: T,
	dynamic: Option<Dyn>,
	ownership: Ownership,
}

#[derive(Clone, Copy, PartialEq)]
enum Ownership {
	Owned,
	Borrow(And),
	MutBorrow(And, Mut),
}
