use itertools::Itertools;
use proc_macro2 as pm2;
use quote::quote;
use regex_automata::RegexBuilder;

pub fn compile(pattern: &str) -> anyhow::Result<pm2::TokenStream> {
	let re = RegexBuilder::new().anchored(true).build_sparse(pattern)?;

	let fwd = re.forward().to_u32()?.to_bytes_native_endian()?;
	let fwd = fwd
		.iter()
		.map(|b| pm2::Literal::u8_unsuffixed(*b))
		.collect_vec();
	let fwd = quote! { &[#(#fwd),*] };

	let rev = re.reverse().to_u32()?.to_bytes_native_endian()?;
	let rev = rev
		.iter()
		.map(|b| pm2::Literal::u8_unsuffixed(*b))
		.collect_vec();
	let rev = quote! { &[#(#rev),*] };

	let regex = quote! {{
		let fwd = unsafe { SparseDFA::from_bytes(#fwd) };
		let rev = unsafe { SparseDFA::from_bytes(#rev) };

		Regex::from_dfas(fwd, rev)
	}};

	Ok(regex)
}
