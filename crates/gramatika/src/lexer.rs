//! This module defines the [`Lexer`] and [`Token`] traits that lay the
//! groundwork for parsing with Gramatika. In this documentation, we'll look at
//! some less trivial `Token` examples and explore how the generated lexer
//! tokenizes input.
//!
//! ## Defining a token
//!
//! Each variant of your `Token` enum should be a tuple variant wrapping a
//! [`Substr`] and [`Span`]:
//! ```
//! #[macro_use]
//! extern crate gramatika;
//!
//! # fn main() {
//! use gramatika::{Span, Substr};
//!
//! #[derive(Token, Lexer, PartialEq)]
//! enum Token {
//! #    #[discard]
//! #    #[pattern = "//.*"]
//! #    LineComment(Substr, Span),
//! #
//! #    #[discard]
//! #    #[multiline]
//! #    #[pattern = r"/\*.*?\*/"]
//! #    BlockComment(Substr, Span),
//! #
//! #    #[subset_of(Ident)]
//! #    #[pattern = "if|else|switch|case|break|for|while|var|print"]
//! #    Keyword(Substr, Span),
//! #
//! #    #[pattern = "[a-zA-Z_][a-zA-Z_0-9]*"]
//!     Ident(Substr, Span),
//! #
//! #    #[pattern = r"[(){}\[\]]"]
//! #    Brace(Substr, Span),
//! #
//! #    #[pattern = "[,.;]"]
//! #    Punct(Substr, Span),
//! #
//! #    #[pattern = "[=!<>]=?"]
//! #    #[pattern = "[-+*/]"]
//! #    Operator(Substr, Span),
//! #
//! #    #[pattern = "(0[xb])?[0-9A-Fa-f][0-9A-Fa-f.]*"]
//! #    NumLiteral(Substr, Span),
//! #
//! #    #[pattern = r#""[^"]+""#]
//! #    StrLiteral(Substr, Span),
//! }
//! # }
//! ```
//! * The [`Substr`] portion (which we'll refer to as the _lexeme_) is an atomic
//!   reference-counted view into the original source string, provided by the
//!   [`arcstr`] crate. These can be `clone`d for very little cost, because only
//!   the pointer to the original string is copied, not the underlying string
//!   itself.
//!
//!   ```
//!   # use gramatika::{ArcStr, Substr};
//!   let source = ArcStr::from("foo bar baz");
//!   {
//!       let foo = source.substr(..3);
//!       let baz = source.substr(8..);
//!
//!       assert_eq!(foo, "foo");
//!       assert_eq!(baz, "baz");
//!       assert!(ArcStr::ptr_eq(foo.parent(), baz.parent()));
//!       assert_eq!(ArcStr::strong_count(&source), Some(3));
//!   }
//!   assert_eq!(ArcStr::strong_count(&source), Some(1));
//!   ```
//!
//! * The [`Span`] indicates the token's location in the original source
//!   document by line and character number.
//!
//!   It's important to note that while the actual values stored in the `Span`
//!   are zero-indexed, printing the `Span` with the `Debug` trait will display
//!   _one-indexed_ values to match the conventions of most code and text
//!   editors.
//!
//!   ```
//!   # use gramatika::Span;
//!   let span = Span::new((0, 0), (0, 4));
//!   let printed = format!("{span:?}");
//!   assert_eq!(printed, "1:1..1:5");
//!   ```
//!
//! When the `Token` and [`Spanned`] traits are in scope, the lexeme and span
//! can be extracted from a `Token` without needing to pattern-match. You can
//! also grab them both in one go with the generated `as_inner` method.
//!
//! ```
//! #[macro_use]
//! extern crate gramatika;
//!
//! # fn main() {
//! use gramatika::{Span, Spanned, Substr, Token as _, span};
//!
//! #[derive(Token, Lexer, PartialEq)]
//! enum Token {
//!     Ident(Substr, Span),
//! }
//!
//! let my_ident = Token::Ident("foo".into(), span![1:1..1:4]);
//!
//! assert_eq!(my_ident.lexeme(), "foo");
//! assert_eq!(my_ident.span(), span![1:1..1:4]);
//!
//! let (lexeme, span) = my_ident.as_inner();
//! assert_eq!(lexeme, my_ident.lexeme());
//! assert_eq!(span, my_ident.span());
//! # }
//! ```
//!
//! ### Debug and Display
//!
//! It's a good idea to derive [`DebugLispToken`] for the enum, and implement
//! both [`Debug`](core::fmt::Debug) and [`Display`](core::fmt::Display):
//!
//! ```
//! #[macro_use]
//! extern crate gramatika;
//!
//! # fn main() {
//! use core::fmt;
//! use gramatika::{Span, Spanned, Substr, Token as _, span};
//!
//! #[derive(Token, Lexer, DebugLispToken, PartialEq)]
//! enum Token {
//!     Ident(Substr, Span),
//! }
//!
//! impl fmt::Display for Token {
//!     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//!         write!(f, "{}", self.lexeme())
//!     }
//! }
//!
//! impl fmt::Debug for Token {
//!     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//!         <Self as gramatika::DebugLisp>::fmt(self, f, 0)
//!     }
//! }
//!
//! let my_ident = Token::Ident("foo".into(), span![1:1..1:4]);
//!
//! let display = format!("{my_ident}");
//! assert_eq!(display, "foo");
//!
//! let debug = format!("{my_ident:?}");
//! assert_eq!(debug, "`foo` (Ident (1:1..1:4))");
//! # }
//! ```
//! [`DebugLispToken`]: gramatika_macro::DebugLispToken
//!
//! ## Configuring the Lexer
//!
//! The real power of Gramatika comes from its lexer generator. Let's define a
//! pattern for our identifier token:
//!
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main() {
//! # use core::fmt;
//! # use gramatika::{Span, Spanned, Substr, Token as _, span};
//! #
//! // ...
//! #[derive(Token, Lexer, DebugLispToken, PartialEq)]
//! enum Token {
//!     #[pattern = "[a-zA-Z_][a-zA-Z_0-9]*"]
//!     Ident(Substr, Span),
//! }
//! #
//! # impl fmt::Display for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         write!(f, "{}", self.lexeme())
//! #     }
//! # }
//! #
//! # impl fmt::Debug for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         <Self as gramatika::DebugLisp>::fmt(self, f, 0)
//! #     }
//! # }
//!
//! let input = "
//!     foo bar baz
//!     foobar
//!     loremIpsum
//!     dolor_sit_amet
//! ";
//! let mut lexer = Lexer::new(input.into());
//! let tokens = lexer.scan();
//!
//! assert_eq!(tokens.len(), 6);
//! assert_eq!(&tokens[0], &Token::Ident("foo".into(), span![2:5..2:8]));
//! assert_eq!(&tokens[5], &Token::Ident("dolor_sit_amet".into(), span![5:5..5:19]));
//! # }
//! ```
//!
//! The `#[pattern]` attribute accepts the same syntax and features as the Rust
//! [`regex` crate]. Those patterns are compiled to [deterministic finite automata]
//!  and used by the generated lexer to find token matches in an input string.
//!
//! [`regex` crate]: https://docs.rs/regex/latest/regex/
//! [deterministic finite automata]: https://swtch.com/~rsc/regexp/regexp1.html
//!
//! ### Unrecognized input
//!
//! If the lexer receives any input that it doesn't know how to handle, it
//! panics. That's not ideal, so to avoid that we'll define a catch-all
//! `Unrecognized` token that eats up all non-whitespace characters. Our
//! [`Parse`] implementations can then handle those gracefully by emitting a
//! useful error message for the user.
//!
//! [`Parse`]: crate::Parse
//!
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main() {
//! # use core::fmt;
//! # use gramatika::{Span, Spanned, Substr, Token as _, span};
//! #
//! // ...
//! #[derive(Token, Lexer, DebugLispToken, PartialEq)]
//! enum Token {
//!     #[pattern = "[a-zA-Z_][a-zA-Z_0-9]*"]
//!     Ident(Substr, Span),
//!
//!     #[pattern = r"\S+"]
//!     Unrecognized(Substr, Span),
//! }
//! #
//! # impl fmt::Display for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         write!(f, "{}", self.lexeme())
//! #     }
//! # }
//! #
//! # impl fmt::Debug for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         <Self as gramatika::DebugLisp>::fmt(self, f, 0)
//! #     }
//! # }
//!
//! let input = "foo 42";
//! let mut lexer = Lexer::new(input.into());
//! let tokens = lexer.scan();
//!
//! assert_eq!(tokens.len(), 2);
//! assert_eq!(&tokens[0], &Token::Ident("foo".into(), span![1:1..1:4]));
//! assert_eq!(&tokens[1], &Token::Unrecognized("42".into(), span![1:5..1:7]));
//! # }
//! ```
//!
//! ### Discarding input
//!
//! Often we want to _recognize_ some input, especially input that might be
//! valid at any location in a source file, without needing to manually deal
//! with those tokens in our [`Parse`] implementations. Essentially, we want to
//! _discard_ that input. The lexer automatically does thie by default for
//! whitespace characters, but we can expand that functionality to any other
//! syntax that we want to ignore completely:
//!
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main() {
//! # use core::fmt;
//! # use gramatika::{Span, Spanned, Substr, Token as _, span};
//! #
//! // ...
//! #[derive(Token, Lexer, DebugLispToken, PartialEq)]
//! enum Token {
//!     #[discard]
//!     #[pattern = "//.*"]
//!     Comment(Substr, Span),
//!     // ...
//! #     #[pattern = "[a-zA-Z_][a-zA-Z_0-9]*"]
//! #     Ident(Substr, Span),
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! }
//! #
//! # impl fmt::Display for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         write!(f, "{}", self.lexeme())
//! #     }
//! # }
//! #
//! # impl fmt::Debug for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         <Self as gramatika::DebugLisp>::fmt(self, f, 0)
//! #     }
//! # }
//!
//! let input = "
//!     // Here's a comment
//!     foo // Here's another one
//! ";
//!
//! let mut lexer = Lexer::new(input.into());
//! let tokens = lexer.scan();
//!
//! assert_eq!(tokens.len(), 1);
//! assert_eq!(&tokens[0], &Token::Ident("foo".into(), span![3:5..3:8]));
//! # }
//! ```
//!
//! ### Matching tokens across multiple lines
//!
//! We may also want to define some tokens that can span multiple lines, but by
//! default a regular expression's `.` doesn't match newline characters. We can
//! change that by adding the `#[multiline]` attribute:
//!
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main() {
//! # use core::fmt;
//! # use gramatika::{Span, Spanned, Substr, Token as _, span};
//! #
//! // ...
//! #[derive(Token, Lexer, DebugLispToken, PartialEq)]
//! enum Token {
//!     #[discard]
//!     #[multiline]
//!     #[pattern = r"/\*.*?\*/"]
//!     BlockComment(Substr, Span),
//!
//!     #[discard]
//!     #[pattern = "//.*"]
//!     LineComment(Substr, Span),
//!     // ...
//! #     #[pattern = "[a-zA-Z_][a-zA-Z_0-9]*"]
//! #     Ident(Substr, Span),
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! }
//! #
//! # impl fmt::Display for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         write!(f, "{}", self.lexeme())
//! #     }
//! # }
//! #
//! # impl fmt::Debug for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         <Self as gramatika::DebugLisp>::fmt(self, f, 0)
//! #     }
//! # }
//!
//! let input = "
//!     /*
//!     Here's a block comment.
//!     It can span as many lines as we please!
//!     */
//!     foo // Here's a line comment
//! ";
//!
//! let mut lexer = Lexer::new(input.into());
//! let tokens = lexer.scan();
//!
//! assert_eq!(tokens.len(), 1);
//! assert_eq!(&tokens[0], &Token::Ident("foo".into(), span![6:5..6:8]));
//! # }
//! ```
//! ### Matching keywords
//!
//! Keywords are a tricky thing, because they will almost certainly overlap with
//! your language's "identifier" token. The matching of tokens is prioritized by
//! declaration order from top to bottom, so you might think we could just
//! declare our `Keyword` token first, but that has an unfortunate consequence:
//!
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main() {
//! # use core::fmt;
//! # use gramatika::{Span, Spanned, Substr, Token as _, span};
//! #
//! // ...
//! #[derive(Token, Lexer, DebugLispToken, PartialEq)]
//! enum Token {
//!     #[pattern = "if|else|for|in|switch|case|break"]
//!     Keyword(Substr, Span),
//!
//!     #[pattern = "[a-zA-Z_][a-zA-Z_0-9]*"]
//!     Ident(Substr, Span),
//! #
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! }
//! #
//! # impl fmt::Display for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         write!(f, "{}", self.lexeme())
//! #     }
//! # }
//! #
//! # impl fmt::Debug for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         <Self as gramatika::DebugLisp>::fmt(self, f, 0)
//! #     }
//! # }
//!
//! let input = "
//!     for foo in bar
//!         intent
//! ";
//!
//! let mut lexer = Lexer::new(input.into());
//! let tokens = lexer.scan();
//!
//! assert_eq!(tokens, vec![
//!     Token::Keyword("for".into(), span![2:5..2:8]),
//!     Token::Ident("foo".into(),   span![2:9..2:12]),
//!     Token::Keyword("in".into(),  span![2:13..2:15]),
//!     Token::Ident("bar".into(),   span![2:16..2:19]),
//!     // Wait a minute, this is not what we wanted!
//!     Token::Keyword("in".into(),  span![3:9..3:11]),
//!     Token::Ident("tent".into(),  span![3:11..3:15]),
//! ]);
//! # }
//! ```
//!
//! To fix that, we can use the `#[subset_of(Other)]` attribute to specify that
//! a token's pattern overlaps with `Other`'s pattern, and should only match if
//! `Other`'s _entire lexeme_ is _also_ a match for the subset.
//!
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main() {
//! # use core::fmt;
//! # use gramatika::{Span, Spanned, Substr, Token as _, span};
//! #
//! // ...
//! #[derive(Token, Lexer, DebugLispToken, PartialEq)]
//! enum Token {
//!     #[subset_of(Ident)]
//!     #[pattern = "if|else|for|in|switch|case|break"]
//!     Keyword(Substr, Span),
//!
//!     #[pattern = "[a-zA-Z_][a-zA-Z_0-9]*"]
//!     Ident(Substr, Span),
//! #
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! }
//! #
//! # impl fmt::Display for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         write!(f, "{}", self.lexeme())
//! #     }
//! # }
//! #
//! # impl fmt::Debug for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         <Self as gramatika::DebugLisp>::fmt(self, f, 0)
//! #     }
//! # }
//!
//! let input = "
//!     for foo in bar
//!         intent
//! ";
//!
//! let mut lexer = Lexer::new(input.into());
//! let tokens = lexer.scan();
//!
//! assert_eq!(tokens, vec![
//!     Token::Keyword("for".into(),  span![2:5..2:8]),
//!     Token::Ident("foo".into(),    span![2:9..2:12]),
//!     Token::Keyword("in".into(),   span![2:13..2:15]),
//!     Token::Ident("bar".into(),    span![2:16..2:19]),
//!     // That's better!
//!     Token::Ident("intent".into(), span![3:9..3:15]),
//! ]);
//! # }
//! ```
//!
//! ### Composing complex patterns
//!
//! Regular expressions are not known for their readability in the best of
//! circumstances, but that's especially true for long patterns with lots of
//! "or" branches. We can define multiple `#[pattern]` attributes for a single
//! token to _compose_ those patterns into a single regular expression:
//!
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main() {
//! # use core::fmt;
//! # use gramatika::{Span, Spanned, Substr, Token as _, span};
//! #
//! // ...
//! #[derive(Token, Lexer, DebugLispToken, PartialEq)]
//! enum Token {
//!     #[pattern = r"[0-9]*\.[0-9]+"]
//!     #[pattern = r"[0-9]+\.[0-9]*"]
//!     FloatLiteral(Substr, Span),
//! #
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! }
//! #
//! # impl fmt::Display for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         write!(f, "{}", self.lexeme())
//! #     }
//! # }
//! #
//! # impl fmt::Debug for Token {
//! #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! #         <Self as gramatika::DebugLisp>::fmt(self, f, 0)
//! #     }
//! # }
//!
//! let input = "
//!     3.141
//!     .25
//!     50.
//! ";
//!
//! let mut lexer = Lexer::new(input.into());
//! let tokens = lexer.scan();
//!
//! assert_eq!(tokens, vec![
//!     Token::FloatLiteral("3.141".into(), span![2:5..2:10]),
//!     Token::FloatLiteral(".25".into(),   span![3:5..3:8]),
//!     Token::FloatLiteral("50.".into(),   span![4:5..4:8]),
//! ]);
//! # }
//! ```
//! The pattern above is exactly equivalent to `[0-9]*\.[0-9]+|[0-9]+\.[0-9]*`,
//! but by putting them on separate lines we can more easily tell the difference
//! between them (the first makes the digits _before_ the `.` optional, while
//! the second does the same for digits _after_ the `.`).
//!

use std::fmt;

use arcstr::{ArcStr, Substr};

use crate::{Span, Spanned};

/// A lexer (AKA scanner, AKA tokenizer) is the piece of the parsing toolchain
/// that takes raw input (e.g., the text of a source file) and "scans" it into
/// discrete chunks of meaningful information (i.e., [tokens]).
///
/// In Gramatika, [parsing] is usually performed on a stream of tokens that are
/// scanned on-demand by a compile-time-generated type implementing this trait.
/// Except perhaps for unit-testing, you should rarely need to interact with the
/// lexer directly.
///
/// [tokens]: crate::Token
/// [parsing]: crate::parse
pub trait Lexer {
	/// The concrete type of [`Token`] this lexer should scan.
	type Output: Token;

	/// Create a new lexer that can scan the provided `input` to a stream of
	/// [`Output`] tokens.
	///
	/// [`Output`]: Lexer::Output
	fn new(input: ArcStr) -> Self;

	/// Experimental
	#[doc(hidden)]
	#[allow(unused_variables)]
	fn with_runtime_matcher<F>(self, matcher: F) -> Self
	where
		Self: Sized,
		F: Fn(&str) -> Option<(usize, <Self::Output as Token>::Kind)> + 'static,
	{
		self
	}

	/// Returns an owned copy of the input [`ArcStr`] this lexer is scanning.
	fn source(&self) -> ArcStr;

	/// Scans a single token from the input.
	///
	/// The implementation generated by the [`derive macro`] does this lazily,
	/// only checking the input for a match when this method is called, and
	/// stopping as soon as the first match is (or isn't) found.
	///
	/// [`derive macro`]: gramatika_macro::Lexer
	fn scan_token(&mut self) -> Option<Self::Output>;

	/// Eagerly scans the entire input to an array of tokens.
	///
	/// Most of the time you'll want to use [`scan_token`] instead to _stream_
	/// tokens from the input on an as-needed basis.
	///
	/// [`scan_token`]: Lexer::scan_token
	fn scan(&mut self) -> Vec<Self::Output> {
		let mut result = vec![];
		while let Some(token) = self.scan_token() {
			result.push(token);
		}
		result
	}
}

/// In the parlance of language parsing, "tokens" are the smallest discrete
/// chunks of meaningful information that can be extracted from some raw input,
/// like the text of a source file.
///
/// If the individual characters of that text are thought of as _atoms_, then
/// its "tokens" could be considered the _molecules_: words, punctuation marks,
/// mathematical operators, etc.
///
/// In Gramatika, this trait is usually [derived] (along with its [`Lexer`]) for
/// some user-defined enum type, with regular-expression patterns specifying
/// the forms that can be taken by each of its variants. See the
/// [module-level documentation] for (much) more detail.
///
/// [derived]: gramatika_macro::Token
/// [module-level documentation]: crate::lexer
pub trait Token
where Self: Clone + Spanned
{
	type Kind: fmt::Debug + PartialEq;

	/// Returns the actual text content of a token.
	///
	/// ```
	/// #[macro_use]
	/// extern crate gramatika;
	///
	/// use gramatika::{
	///     arcstr::literal_substr,
	///     Substr, Span, Token as _,
	/// };
	///
	/// # fn main() {
	/// #[derive(Token, Lexer)]
	/// enum Token {
	///     #[subset_of(Ident)]
	///     #[pattern = "var"]
	///     Keyword(Substr, Span),
	///
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	///
	///     #[pattern = "[0-9]+"]
	///     IntLiteral(Substr, Span),
	///
	///     #[pattern = "="]
	///     Operator(Substr, Span),
	///
	///     #[pattern = ";"]
	///     Punct(Substr, Span),
	/// }
	///
	/// let src = "var the_answer = 42;";
	/// let tokens = Lexer::new(src.into()).scan();
	///
	/// assert_eq!(tokens[1].lexeme(), literal_substr!("the_answer"));
	/// # }
	/// ```
	fn lexeme(&self) -> Substr;

	/// Returns the [`Kind`] of this token. Used in [`ParseStreamer`] methods like
	/// [`check_kind`] and [`consume_kind`]. Effectively a more user-friendly version of
	/// [`std::mem::discriminant`].
	///
	/// [`Kind`]: Token::Kind
	/// [`ParseStreamer`]: crate::parse::ParseStreamer
	/// [`check_kind`]: crate::parse::ParseStreamer::check_kind
	/// [`consume_kind`]: crate::parse::ParseStreamer::consume_kind
	///
	/// ```
	/// #[macro_use]
	/// extern crate gramatika;
	///
	/// use gramatika::{Substr, Span, Token as _};
	///
	/// # fn main() {
	/// #[derive(Token, Lexer)]
	/// enum Token {
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	/// }
	///
	/// let input = "foo";
	/// let token = Lexer::new(input.into())
	///     .scan_token()
	///     .expect("Expected to match Ident `foo`");
	///
	/// assert_eq!(token.kind(), TokenKind::Ident);
	/// # }
	/// ```
	fn kind(&self) -> Self::Kind;

	/// Decomposes the token into its constituent [`Kind`], [`lexeme`] and
	/// [`Span`]) parts, with the `lexeme` as a [`&str`] slice for compatibility
	/// with pattern matching.
	///
	/// [`Kind`]: Token::Kind
	/// [`lexeme`]: Token::lexeme
	/// [`span`]: crate::Span
	/// [`&str`]: prim@str
	///
	/// ```
	/// #[macro_use]
	/// extern crate gramatika;
	///
	/// use gramatika::{Substr, Span, Token as _};
	///
	/// # fn main() {
	/// #[derive(Token, Lexer)]
	/// enum Token {
	///     #[pattern = "[a-zA-Z_][a-zA-Z0-9_]*"]
	///     Ident(Substr, Span),
	/// }
	///
	/// let input = "foo";
	/// let token = Lexer::new(input.into())
	///     .scan_token()
	///     .expect("Expected to match Ident `foo`");
	///
	/// assert!(matches!(token.as_matchable(), (TokenKind::Ident, "foo", _)));
	/// # }
	/// ```
	fn as_matchable(&self) -> (Self::Kind, &str, Span);
}
