use proc_macro::TokenStream;
use proc_macro2 as pm2;
use proc_macro2::Ident;
use quote::quote;
use syn::{
	parse_macro_input,
	token::{And, Dyn, Mut},
	Token,
};

mod codegen;
mod parse;

pub fn visitor(input: TokenStream) -> TokenStream {
	let def = parse_macro_input!(input as VisitorDef);
	let output = quote!(#def);

	output.into()
}

pub fn walker(input: TokenStream) -> TokenStream {
	let def = parse_macro_input!(input as WalkerDef);
	let output = quote!(#def);

	output.into()
}

struct VisitorDef {
	visitor_ident: Ident,
	walker_ident: Option<Ident>,
	receiver: Annotated<Token![self]>,
	signatures: Vec<VisitorSignature>,
}

struct VisitorSignature {
	receiver: Annotated<Token![self]>,
	param_type: Annotated<Ident>,
	ret: Option<Annotated<Ident>>,
}

struct WalkerDef {
	target: Ident,
	methods: Vec<WalkerMethod>,
}

struct WalkerMethod {
	walker_type: Ident,
	receiver: Annotated<Token![self]>,
	visitor_name: Ident,
	visitor_type: Annotated<Ident>,
	body: pm2::TokenStream,
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
