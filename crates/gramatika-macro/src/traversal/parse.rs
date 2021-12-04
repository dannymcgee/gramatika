use itertools::Itertools;
use proc_macro2::Ident;
use syn::{
	braced, parenthesized,
	parse::{Parse, ParseStream},
	token::SelfValue,
	Token,
};

use super::{Annotated, Ownership, Signature, VisitorDef};

struct PartialSignature {
	param_type: Annotated<Ident>,
	ret: Option<Annotated<Ident>>,
}

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
			.parse_terminated::<_, Token![;]>(PartialSignature::parse)?
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

impl Parse for PartialSignature {
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

impl PartialSignature {
	fn with_receiver(self, receiver: Annotated<SelfValue>) -> Signature {
		Signature {
			receiver,
			param_type: self.param_type,
			ret: self.ret,
		}
	}
}
