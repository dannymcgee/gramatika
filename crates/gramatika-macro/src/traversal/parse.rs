use std::iter;

use convert_case::{Case, Casing};
use itertools::Itertools;
use pm2::{Group, Span, TokenTree};
use proc_macro2 as pm2;
use proc_macro2::Ident;
use quote::format_ident;
use syn::{
	braced, parenthesized,
	parse::{Parse, ParseStream},
	token::{Brace, SelfValue},
	Token,
};

use super::{
	Annotated, Ownership, VisitorDef, VisitorSignature, WalkerDef, WalkerMethod,
};

impl Parse for VisitorDef {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let visitor_ident = input.parse::<Ident>()?;
		let walker_ident = match input.parse::<Token![,]>() {
			Ok(_) => Some(input.parse::<Ident>()?),
			Err(_) => None,
		};
		let _ = input.parse::<Token![for]>()?;
		let receiver = input.parse::<Annotated<Token![self]>>()?;

		let content;
		braced!(content in input);

		let signatures = content
			.parse_terminated::<_, Token![;]>(PartialVisitorSignature::parse)?
			.into_iter()
			.map(|sig| sig.with_receiver(receiver.clone()))
			.collect::<Vec<_>>();

		if walker_ident.is_some() {
			for (prev, current) in signatures.iter().map(|sig| &sig.param_type).tuples() {
				if current.ownership != prev.ownership {
					return Err(syn::Error::new_spanned(
						&current.ident,
						"The ownership of all parameter types must agree if a walker trait is generated."
					));
				}
			}
		}

		Ok(Self {
			visitor_ident,
			walker_ident,
			receiver,
			signatures,
		})
	}
}

struct PartialVisitorSignature {
	param_type: Annotated<Ident>,
	ret: Option<Annotated<Ident>>,
}

impl Parse for PartialVisitorSignature {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let _ = input.parse::<Token![fn]>()?;

		let params;
		parenthesized!(params in input);
		let param_type = params.parse::<Annotated<Ident>>()?;

		let ret = match input.parse::<Token![->]>() {
			Ok(_) => Some(input.parse::<Annotated<Ident>>()?),
			Err(_) => None,
		};

		Ok(Self { param_type, ret })
	}
}

impl PartialVisitorSignature {
	fn with_receiver(self, receiver: Annotated<SelfValue>) -> VisitorSignature {
		VisitorSignature {
			receiver,
			param_type: self.param_type,
			ret: self.ret,
		}
	}
}

impl Parse for WalkerDef {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		input.parse::<Token![for]>()?;
		let target = input.parse::<Ident>()?;
		input.parse::<Token![where]>()?;

		let mut methods = vec![];
		while !input.peek(Brace) {
			methods.push(input.parse::<WalkerMethodBuilder>()?);
			let _ = input.parse::<Token![,]>();
		}

		let content;
		braced!(content in input);

		let methods = methods
			.into_iter()
			.map(|method| method.finish(content.cursor().token_stream()).unwrap())
			.collect();

		content.step(|cursor| {
			let mut current = *cursor;
			while let Some((_, next)) = current.token_tree() {
				current = next;
			}
			Ok(((), current))
		})?;

		Ok(Self { target, methods })
	}
}

struct WalkerMethodBuilder {
	walker_type: Ident,
	method_name: Ident,
	receiver: Annotated<Token![self]>,
	visitor_name: Ident,
	visitor_type: Annotated<Ident>,
}

impl Parse for WalkerMethodBuilder {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let walker_type = input.parse::<Ident>()?;
		input.parse::<Token![:]>()?;
		input.parse::<Token![fn]>()?;

		let method_name =
			format_ident!("{}", walker_type.to_string().to_case(Case::Snake));

		let params;
		parenthesized!(params in input);

		let receiver = params.parse::<Annotated<Token![self]>>()?;
		params.parse::<Token![,]>()?;

		let visitor_name = params.parse::<Ident>()?;
		params.parse::<Token![:]>()?;
		let visitor_type = params.parse::<Annotated<Ident>>()?;

		Ok(Self {
			walker_type,
			method_name,
			receiver,
			visitor_name,
			visitor_type,
		})
	}
}

impl WalkerMethodBuilder {
	fn finish(self, input: pm2::TokenStream) -> syn::Result<WalkerMethod> {
		let mut stream = input.into_iter();
		let mut output = pm2::TokenStream::new();

		while let Some(tt) = stream.next() {
			let result = match tt {
				TokenTree::Punct(punct) if punct.as_char() == '$' => {
					let next = stream.next().unwrap();
					self.replace_ident(next)
				}
				TokenTree::Group(group) => self.expand_group(group),
				_ => Ok(tt),
			};

			match result {
				Ok(tt) => output.extend(iter::once(tt)),
				Err(msg) => return Err(syn::Error::new(Span::call_site(), msg)),
			}
		}

		Ok(WalkerMethod {
			walker_type: self.walker_type,
			receiver: self.receiver,
			visitor_name: self.visitor_name,
			visitor_type: self.visitor_type,
			body: output,
		})
	}

	fn replace_ident(&self, tt: TokenTree) -> Result<TokenTree, String> {
		match tt {
			TokenTree::Ident(ident) if ident == "walk" => {
				Ok(TokenTree::Ident(self.method_name.clone()))
			}
			other => Err(format!(
				"Expected template variable `$walk`, found: `${}`",
				other
			)),
		}
	}

	fn expand_group(&self, group: Group) -> Result<TokenTree, String> {
		let delim = group.delimiter();
		let mut input = group.stream().into_iter();
		let mut output = pm2::TokenStream::new();

		while let Some(tt) = input.next() {
			match tt {
				TokenTree::Punct(punct) if punct.as_char() == '$' => {
					let next = input.next().unwrap();
					output.extend(iter::once(self.replace_ident(next)?));
				}
				TokenTree::Group(group) => {
					output.extend(iter::once(self.expand_group(group)?));
				}
				_ => output.extend(iter::once(tt)),
			}
		}

		Ok(TokenTree::Group(Group::new(delim, output)))
	}
}

impl<T> Parse for Annotated<T>
where T: Parse
{
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let ownership = match input.parse::<Token![&]>() {
			Ok(and) => match input.parse::<Token![mut]>() {
				Ok(mut_) => Ownership::MutBorrow(and, mut_),
				Err(_) => Ownership::Borrow(and),
			},
			Err(_) => Ownership::Owned,
		};

		let dynamic = match input.parse::<Token![dyn]>() {
			Ok(dyn_) => Some(dyn_),
			Err(_) => None,
		};

		let ident = input.parse::<T>()?;

		Ok(Self {
			ident,
			dynamic,
			ownership,
		})
	}
}
