use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DataEnum, DeriveInput};

use crate::common;

pub fn derive(input: TokenStream) -> TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	let enum_ident = &ast.ident;
	let vis = &ast.vis;
	let lexer_ident = format_ident!("Lexer");
	let generics = &ast.generics;
	let lifetime = common::lifetime(generics);

	let (variant_ident, _, _) = match &ast.data {
		Data::Enum(DataEnum { variants, .. }) => common::expand_variants(variants),
		_ => unimplemented!(),
	};

	let (ctor_ident, matcher_ident) = common::token_funcs(&variant_ident);

	let result = quote! {
		use ::gramatika::Lexer as _;

		#vis struct #lexer_ident<#lifetime> {
			input: &#lifetime str,
			remaining: &#lifetime str,
			current: ::gramatika::Position,
			lookahead: ::gramatika::Position,
		}

		type __TOKEN_CTOR<#lifetime> = fn(&#lifetime str, ::gramatika::Span) -> #enum_ident#generics;

		impl<#lifetime> ::gramatika::Lexer for #lexer_ident<#lifetime> {
			type Input = &#lifetime str;
			type Output = #enum_ident#generics;

			fn new(input: Self::Input) -> Self {
				Self {
					input,
					remaining: input,
					current: ::gramatika::Position::default(),
					lookahead: ::gramatika::Position::default(),
				}
			}

			fn source(&self) -> Self::Input {
				self.input
			}

			fn scan(&mut self) -> ::std::vec::Vec<Self::Output> {
				let mut output = vec![];
				while let Some(token) = self.scan_token() {
					output.push(token);
				}

				output
			}

			fn scan_token(&mut self) -> Option<Self::Output> {
				None#(
					.or_else(|| #enum_ident::#matcher_ident(self.remaining)
						.map(|m| (m, #enum_ident::#ctor_ident as __TOKEN_CTOR)))
				)*
				.map(|(m, ctor)| {
					self.lookahead.character += m.end();

					let lexeme = m.as_str();
					let span = ::gramatika::Span {
						start: self.current,
						end: self.lookahead,
					};
					let token = ctor(lexeme, span);

					self.remaining = &self.remaining[m.end()..];
					self.current = self.lookahead;

					token
				})
				.or_else(|| self.remaining.chars().peekable().peek().and_then(|c| match c {
					' ' | '\t' | '\r' => {
						self.lookahead.character += 1;
						self.current.character += 1;
						self.remaining = &self.remaining[1..];

						self.scan_token()
					},
					'\n' => {
						self.lookahead.line += 1;
						self.lookahead.character = 0;
						self.current = self.lookahead;
						self.remaining = &self.remaining[1..];

						self.scan_token()
					},
					other => panic!("Unsupported input: `{}`", other),
				}))
			}
		}
	};

	result.into()
}
