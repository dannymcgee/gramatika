use proc_macro::TokenStream;

mod common;
mod debug_lisp;
mod lexer;
mod parse;
mod token;

#[proc_macro_derive(Token, attributes(pattern))]
pub fn derive_token(input: TokenStream) -> TokenStream {
	token::derive(input)
}

#[proc_macro_derive(Lexer)]
pub fn derive_lexer(input: TokenStream) -> TokenStream {
	lexer::derive(input)
}

#[proc_macro_derive(Parse, attributes(parse_token, token, token_kind))]
pub fn derive_parse(input: TokenStream) -> TokenStream {
	parse::derive(input)
}

#[proc_macro_derive(DebugLisp)]
pub fn derive_debug_lisp(input: TokenStream) -> TokenStream {
	debug_lisp::derive(input)
}

#[proc_macro_derive(DebugLispToken)]
pub fn derive_debug_lisp_token(input: TokenStream) -> TokenStream {
	debug_lisp::derive_token(input)
}
