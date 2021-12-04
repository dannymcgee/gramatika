#![allow(non_snake_case)]

use convert_case::{Case, Casing};
use pm2::Ident;
use proc_macro2 as pm2;
use quote::{format_ident, quote, ToTokens};
use syn::Token;

use super::{Annotated, Ownership, VisitorDef, VisitorSignature, WalkerDef};

impl ToTokens for VisitorDef {
	fn to_tokens(&self, tokens: &mut pm2::TokenStream) {
		let Visitor = &self.visitor_ident;
		let signatures = self.signatures.iter();

		tokens.extend(quote! {
			#[allow(unused_variables)]
			pub trait #Visitor {
				#(#signatures)*
			}
		});

		if let Some(Walk) = &self.walker_ident {
			if let Some(signature) = self.signatures.get(0) {
				let swn = signature.param_type.ownership;
				let pwn = self.receiver.ownership;
				let walk = format_ident!("{}", Walk.to_string().to_case(Case::Snake));

				if let Some(dy) = self.receiver.dynamic.as_ref() {
					tokens.extend(quote! {
						pub trait #Walk {
							fn #walk(#swn self, visitor: #pwn #dy #Visitor);
						}
					});
				} else {
					tokens.extend(quote! {
						pub trait #Walk {
							fn #walk<V>(#swn self, visitor: #pwn V)
							where V: #Visitor;
						}
					});
				}
			}
		}
	}
}

impl ToTokens for VisitorSignature {
	fn to_tokens(&self, tokens: &mut pm2::TokenStream) {
		let ParamType = &self.param_type;
		let visit_host =
			format_ident!("visit_{}", ParamType.ident.to_string().to_case(Case::Snake),);
		let slf = &self.receiver;

		if let Some(ReturnType) = self.ret.as_ref() {
			tokens.extend(quote! {
				#[must_use]
				fn #visit_host(#slf, host: #ParamType) -> #ReturnType {
					#ReturnType::default()
				}
			});
		} else {
			tokens.extend(quote! {
				fn #visit_host(#slf, host: #ParamType) {}
			});
		}
	}
}

impl ToTokens for WalkerDef {
	fn to_tokens(&self, tokens: &mut pm2::TokenStream) {
		let Target = &self.target;
		for method in self.methods.iter() {
			let Walk = &method.walker_type;
			let walk = format_ident!("{}", Walk.to_string().to_case(Case::Snake));
			let slf = &method.receiver;
			let visitor = &method.visitor_name;
			let Visitor = &method.visitor_type;
			let body = &method.body;

			tokens.extend(quote! {
				impl #Walk for #Target {
					#[allow(unused_variables)]
					fn #walk(#slf, #visitor: #Visitor) {
						#body
					}
				}
			});
		}
	}
}

impl ToTokens for Annotated<Ident> {
	fn to_tokens(&self, tokens: &mut pm2::TokenStream) {
		let ident = &self.ident;
		let own = &self.ownership;
		let dy = self.dynamic.as_ref();

		tokens.extend(quote! { #own #dy #ident });
	}
}

impl ToTokens for Annotated<Token![self]> {
	fn to_tokens(&self, tokens: &mut pm2::TokenStream) {
		let own = self.ownership;

		tokens.extend(quote! { #own self });
	}
}

impl ToTokens for Ownership {
	fn to_tokens(&self, tokens: &mut pm2::TokenStream) {
		match self {
			Self::Borrow(and) => and.to_tokens(tokens),
			Self::MutBorrow(and, mut_) => tokens.extend(quote! { #and #mut_ }),
			Self::Owned => {}
		}
	}
}
