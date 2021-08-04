pub trait Parse {
	type Stream: ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self, String>
	where Self: Sized;
}

pub trait ParseStream {
	type Token: crate::Token;

	fn parse<P: Parse<Stream = Self>>(&mut self) -> Result<P, String>;
	fn is_empty(&mut self) -> bool;
	fn peek(&mut self) -> Option<&Self::Token>;
	fn check_kind(&mut self, kind: <Self::Token as crate::Token>::Kind) -> bool;
	fn check(&mut self, compare: Self::Token) -> bool;
	fn consume(&mut self, compare: Self::Token) -> Result<Self::Token, String>;
	fn consume_kind(
		&mut self,
		kind: <Self::Token as crate::Token>::Kind,
	) -> Result<Self::Token, String>;
}
