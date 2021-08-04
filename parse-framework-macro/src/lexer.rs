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
		use ::parse_framework::Lexer as _;

		#vis struct #lexer_ident<#lifetime> {
			input: &#lifetime str,
			current: ::parse_framework::Position,
			lookahead: ::parse_framework::Position,
		}

		type __TOKEN_CTOR<#lifetime> = fn(&#lifetime str, ::parse_framework::Span) -> #enum_ident#generics;

		impl<#lifetime> ::parse_framework::Lexer for #lexer_ident<#lifetime> {
			type Input = &#lifetime str;
			type Output = #enum_ident#generics;

			fn new(input: Self::Input) -> Self {
				Self {
					input,
					current: ::parse_framework::Position::default(),
					lookahead: ::parse_framework::Position::default(),
				}
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
					.or_else(|| #enum_ident::#matcher_ident(self.input)
						.map(|m| (m, #enum_ident::#ctor_ident as __TOKEN_CTOR)))
				)*
				.map(|(m, ctor)| {
					self.lookahead.character += m.end();

					let lexeme = m.as_str();
					let span = ::parse_framework::Span {
						start: self.current,
						end: self.lookahead,
					};
					let token = ctor(lexeme, span);

					self.input = &self.input[m.end()..];
					self.current = self.lookahead;

					token
				})
				.or_else(|| self.input.chars().peekable().peek().and_then(|c| match c {
					' ' | '\t' | '\r' => {
						self.lookahead.character += 1;
						self.current.character += 1;
						self.input = &self.input[1..];

						self.scan_token()
					},
					'\n' => {
						self.lookahead.line += 1;
						self.lookahead.character = 0;
						self.current = self.lookahead;
						self.input = &self.input[1..];

						self.scan_token()
					},
					other => panic!("Unsupported input: `{}`", other),
				}))
			}
		}
	};

	result.into()
}
