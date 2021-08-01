use proc_macro::TokenStream;

mod common;
mod lexer;
mod token;

#[proc_macro_derive(Token, attributes(pattern))]
pub fn derive_token(input: TokenStream) -> TokenStream {
	token::derive(input)
}

#[proc_macro_derive(Lexer)]
pub fn derive_lexer(input: TokenStream) -> TokenStream {
	lexer::derive(input)
}
