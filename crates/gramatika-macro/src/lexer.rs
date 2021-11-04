use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DataEnum, DeriveInput};

use crate::common;

pub fn derive(input: TokenStream) -> TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	let enum_ident = &ast.ident;
	let vis = &ast.vis;
	let lexer_ident = format_ident!("Lexer");

	let (variant_ident, variant_pattern, _) = match &ast.data {
		Data::Enum(DataEnum { variants, .. }) => common::expand_variants(variants),
		_ => unimplemented!(),
	};

	let (ctor_ident, matcher_ident) =
		common::token_funcs(&variant_ident, &variant_pattern);

	let result = quote! {
		use ::gramatika::Lexer as _;

		#vis struct #lexer_ident {
			input: ::gramatika::ArcStr,
			remaining: ::gramatika::Substr,
			current: ::gramatika::Position,
			lookahead: ::gramatika::Position,
		}

		type __TOKEN_CTOR = fn(::gramatika::Substr, ::gramatika::Span) -> #enum_ident;

		impl ::gramatika::Lexer for #lexer_ident {
			type Output = #enum_ident;

			fn new(input: ::gramatika::ArcStr) -> Self {
				Self {
					remaining: input.substr(..),
					input,
					current: ::gramatika::Position::default(),
					lookahead: ::gramatika::Position::default(),
				}
			}

			fn source(&self) -> ::gramatika::ArcStr {
				::gramatika::ArcStr::clone(&self.input)
			}

			fn scan(&mut self) -> ::std::vec::Vec<Self::Output> {
				let mut output = vec![];
				while let Some(token) = self.scan_token() {
					output.push(token);
				}

				output
			}

			fn scan_token(&mut self) -> Option<Self::Output> {
				let result = None
				#(
					.or_else(|| #enum_ident::#matcher_ident(&self.remaining)
						.map(|m| (m, #enum_ident::#ctor_ident as __TOKEN_CTOR)))
				)*;

				match result {
					Some((m, ctor)) => {
						let match_end = m.end();
						let lexeme = self.remaining.substr_from(m.as_str());

						self.lookahead.character += match_end;

						let span = ::gramatika::Span {
							start: self.current,
							end: self.lookahead,
						};
						let token = ctor(lexeme, span);

						self.remaining = self.remaining.substr(match_end..);
						self.current = self.lookahead;

						Some(token)
					}
					None => {
						self.remaining.clone().chars().next().and_then(|c| match c {
							' ' | '\t' | '\r' => {
								self.lookahead.character += 1;
								self.current.character += 1;
								self.remaining = self.remaining.substr(1..);

								self.scan_token()
							},
							'\n' => {
								self.lookahead.line += 1;
								self.lookahead.character = 0;
								self.current = self.lookahead;
								self.remaining = self.remaining.substr(1..);

								self.scan_token()
							},
							other => panic!("Unsupported input: `{}` at {:?}", other, self.current),
						})
					}
				}
			}
		}
	};

	result.into()
}
