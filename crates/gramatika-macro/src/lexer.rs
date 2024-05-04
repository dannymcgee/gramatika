use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

use crate::common;

pub fn derive(input: TokenStream) -> TokenStream {
	let ast = parse_macro_input!(input as DeriveInput);
	let enum_ident = &ast.ident;
	let kind_ident = format_ident!("{}Kind", enum_ident);
	let vis = &ast.vis;
	let lexer_ident = format_ident!("Lexer");

	let variants = common::expand_variants(&ast.data);
	let variant_ident = variants.iter().map(|v| v.ident.clone()).collect::<Vec<_>>();
	let (ctor_ident, matcher_ident) = common::token_funcs(&variants);

	let result = quote! {
		use ::gramatika::Lexer as _;

		#vis struct #lexer_ident {
			input: ::gramatika::ArcStr,
			remaining: ::gramatika::Substr,
			current: ::gramatika::Position,
			lookahead: ::gramatika::Position,
			runtime_matcher: Option<Box<
				dyn Fn(&str) -> Option<(
					usize,
					<<Self as ::gramatika::Lexer>::Output as ::gramatika::Token>::Kind
				)>
			>>
		}

		#[allow(non_camel_case_types)]
		type __TOKEN_CTOR = fn(::gramatika::Substr, ::gramatika::Span) -> #enum_ident;

		impl ::gramatika::Lexer for #lexer_ident {
			type Output = #enum_ident;

			fn new(input: ::gramatika::ArcStr) -> Self {
				Self {
					remaining: input.substr(..),
					input,
					current: ::gramatika::Position::default(),
					lookahead: ::gramatika::Position::default(),
					runtime_matcher: None,
				}
			}

			fn with_runtime_matcher<F>(mut self, matcher: F) -> Self
			where
				F: Fn(&str) -> Option<(
					usize,
					<<Self as ::gramatika::Lexer>::Output as ::gramatika::Token>::Kind
				)> + 'static
			{
				self.runtime_matcher = Some(Box::new(matcher));
				self
			}

			fn source(&self) -> ::gramatika::ArcStr {
				::gramatika::ArcStr::clone(&self.input)
			}

			fn scan_token(&mut self) -> Option<Self::Output> {
				let result = None
				.or_else(|| match self.runtime_matcher.as_ref() {
					Some(matcher) => {
						matcher(&self.remaining).map(|(len, kind)| (0, len, kind))
					}
					None => None,
				})
				#(.or_else(|| #enum_ident::#matcher_ident(&self.remaining)))*;

				match result {
					Some((start, end, kind)) => {
						let ctor = match kind {#(
							#kind_ident::#variant_ident => #enum_ident::#ctor_ident as __TOKEN_CTOR
						),*};
						let lexeme = self.remaining.substr(start..end);

						if #kind_ident::multilines().contains(&kind) {
							let mut line_inc = 0_usize;
							let mut remaining = lexeme.as_str();

							while let Some(idx) = remaining.find('\n') {
								line_inc += 1;
								remaining = &remaining[idx+1..];
							}
							let char_inc = remaining.len();

							self.lookahead.line += line_inc;

							if line_inc > 0 {
								self.lookahead.character = char_inc;
							} else {
								self.lookahead.character += char_inc;
							}
						} else {
							self.lookahead.character += end;
						}

						let span = ::gramatika::Span {
							start: self.current,
							end: self.lookahead,
						};
						let token = ctor(lexeme, span);

						self.remaining = self.remaining.substr(end..);
						self.current = self.lookahead;

						if #kind_ident::discards().contains(&kind) {
							self.scan_token()
						} else {
							Some(token)
						}
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
