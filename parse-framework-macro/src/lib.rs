use proc_macro as pm;

mod token;

#[proc_macro_derive(Token, attributes(pattern))]
pub fn derive(input: pm::TokenStream) -> pm::TokenStream {
	token::derive(input)
}
