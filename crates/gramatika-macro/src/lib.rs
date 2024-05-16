use proc_macro::TokenStream;

mod common;
mod debug_lisp;
mod lexer;
mod regex;
mod token;
mod traversal;

#[proc_macro_derive(Token, attributes(pattern, subset_of, discard, multiline))]
pub fn derive_token(input: TokenStream) -> TokenStream {
	token::derive(input)
}

#[proc_macro_derive(Lexer)]
#[deprecated(since = "0.6.0", note = "Use `gramatika::TokenStream<T>` instead")]
pub fn derive_lexer(input: TokenStream) -> TokenStream {
	lexer::derive(input)
}

#[proc_macro_derive(DebugLisp)]
pub fn derive_debug_lisp(input: TokenStream) -> TokenStream {
	debug_lisp::derive(input)
}

#[proc_macro_derive(DebugLispToken)]
pub fn derive_debug_lisp_token(input: TokenStream) -> TokenStream {
	debug_lisp::derive_token(input)
}

/// Experimental
#[doc(hidden)]
#[proc_macro]
pub fn regex(input: TokenStream) -> TokenStream {
	regex::proc(input)
}

/// Experimental
#[doc(hidden)]
#[proc_macro]
pub fn visitor(input: TokenStream) -> TokenStream {
	traversal::visitor(input)
}

/// Experimental
#[doc(hidden)]
#[proc_macro]
pub fn walker(input: TokenStream) -> TokenStream {
	traversal::walker(input)
}
