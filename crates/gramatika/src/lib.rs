//! # Gramatika
//! A minimal toolkit for writing parsers with Rust
//!
//! ## Getting Started
//! Add the dependency to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! gramatika = "0.5"
//! ```
//!
//! Define an enum for your tokens and derive the [`Token`] trait:
//! ```
//! #[macro_use]
//! extern crate gramatika;
//!
//! # fn main () {
//! use gramatika::{Span, Substr};
//!
//! #[derive(Token)]
//! enum Token {
//!     #[pattern = "print"]
//!     Keyword(Substr, Span),
//!
//!     #[pattern = r#"".*?""#]
//!     StringLiteral(Substr, Span),
//!
//!     #[pattern = ";"]
//!     Punct(Substr, Span),
//!
//!     #[pattern = r"\S+"]
//!     Unrecognized(Substr, Span),
//! }
//! # }
//! ```
//!
//! Next, you'll probably find it useful to declare a type alias for your
//! [`TokenStream`] and [`ParseStream`]:
//!
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main () {
//! # use gramatika::{Span, Substr};
//! #
//! # #[derive(Token)]
//! # enum Token {
//! #     #[pattern = "print"]
//! #     Keyword(Substr, Span),
//! #     #[pattern = r#"".*?""#]
//! #     StringLiteral(Substr, Span),
//! #     #[pattern = ";"]
//! #     Punct(Substr, Span),
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! # }
//! // ...
//! type Lexer = gramatika::TokenStream<Token>;
//! type ParseStream = gramatika::ParseStream<Token, Lexer>;
//! # }
//! ```
//! Then define your syntax tree structure:
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main () {
//! # use gramatika::{Span, Substr};
//! #
//! # #[derive(Token)]
//! # enum Token {
//! #     #[pattern = "print"]
//! #     Keyword(Substr, Span),
//! #     #[pattern = r#"".*?""#]
//! #     StringLiteral(Substr, Span),
//! #     #[pattern = ";"]
//! #     Punct(Substr, Span),
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! # }
//! # type Lexer = gramatika::TokenStream<Token>;
//! # type ParseStream = gramatika::ParseStream<Token, Lexer>;
//! // ...
//! struct Program {
//!     statements: Vec<Stmt>,
//! }
//!
//! enum Stmt {
//!     Empty(Token),
//!     Print(PrintStmt),
//! }
//!
//! struct PrintStmt {
//!     pub keyword: Token,
//!     pub string: Token,
//!     pub terminator: Token,
//! }
//! # }
//! ```
//! Finally, implement the [`Parse`] trait for each node of your syntax tree:
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main () {
//! use gramatika::{Parse, ParseStreamer, Span, SpannedError, Substr, Token as _};
//!
//! #[derive(Debug, Token)]
//! enum Token {
//!     // ...
//! #     #[pattern = "print"]
//! #     Keyword(Substr, Span),
//! #     #[pattern = r#"".*?""#]
//! #     StringLiteral(Substr, Span),
//! #     #[pattern = ";"]
//! #     Punct(Substr, Span),
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! }
//! # type Lexer = gramatika::TokenStream<Token>;
//! # type ParseStream = gramatika::ParseStream<Token, Lexer>;
//! # struct Program {
//! #    statements: Vec<Stmt>,
//! # }
//! # enum Stmt {
//! #     Empty(Token),
//! #     Print(PrintStmt),
//! # }
//! # struct PrintStmt {
//! #     pub keyword: Token,
//! #     pub string: Token,
//! #     pub terminator: Token,
//! # }
//!
//! // ...
//!
//! impl Parse for Program {
//!     type Stream = ParseStream;
//!
//!     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
//!         let mut statements = vec![];
//!         while !input.is_empty() {
//!             // We can call `ParseStream::parse::<T>()` for any `T: Parse`
//!             statements.push(input.parse()?);
//!         }
//!         Ok(Self { statements })
//!     }
//! }
//!
//! impl Parse for Stmt {
//!     type Stream = ParseStream;
//!
//!     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
//!         // Check the next token without advancing the stream
//!         match input.peek() {
//!             // `as_matchable()` cheaply decomposes a token into parts that
//!             // can be be pattern-matched: `(TokenKind, &str, Span)`
//!             Some(token) => match token.as_matchable() {
//!                 // `ParseStream` implements `Iterator<Item = Token>`, so we
//!                 // can call `next()` to consume the token and advance the stream
//!                 (TokenKind::Punct, ";", _) => Ok(Stmt::Empty(input.next().unwrap())),
//!                 // Recursively calling `input.parse()` lets us manage the
//!                 // complexity of parsing deep and complex syntax trees by
//!                 // taking it one small step at a time
//!                 (TokenKind::Keyword, "print", _) => Ok(Stmt::Print(input.parse()?)),
//!                 // Not what we were expecting? `SpannedError` has a `Display`
//!                 // implementation that's perfect for giving useful feedback
//!                 // to users of our language, highlighting the exact place in
//!                 // the source code where the error occurred.
//!                 (_, _, span) => Err(SpannedError {
//!                     message: "Expected `;` or `print`".into(),
//!                     source: input.source(),
//!                     span: Some(span),
//!                 }),
//!             }
//!             None => Err(SpannedError {
//!                 message: "Unexpected end of input".into(),
//!                 source: input.source(),
//!                 span: None,
//!             }),
//!         }
//!     }
//! }
//!
//! impl Parse for PrintStmt {
//!     type Stream = ParseStream;
//!
//!     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
//!         // All of the `ParseStream` methods that return a `Result` will emit
//!         // a `SpannedError` much like the examples above, so we can use the
//!         // `?` operator to consume the tokens we're expecting without
//!         // needing to specify an error message for every single one.
//!         let keyword = input.consume(keyword![print])?;
//!         let string = input.consume_kind(TokenKind::StringLiteral)?;
//!         let terminator = input.consume(punct![;])?;
//!
//!         Ok(PrintStmt {
//!             keyword,
//!             string,
//!             terminator,
//!         })
//!     }
//! }
//! # }
//! ```
//! And now we can give it a proper test run:
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main () {
//! use gramatika::{
//!     Parse, ParseStreamer, Span, Spanned, SpannedError, Substr, Token as _,
//! };
//! # #[derive(Debug, Token)]
//! # enum Token {
//! #     #[pattern = "print"]
//! #     Keyword(Substr, Span),
//! #     #[pattern = r#"".*?""#]
//! #     StringLiteral(Substr, Span),
//! #     #[pattern = ";"]
//! #     Punct(Substr, Span),
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! # }
//! # type Lexer = gramatika::TokenStream<Token>;
//! # type ParseStream = gramatika::ParseStream<Token, Lexer>;
//! #
//! # struct Program {
//! #     statements: Vec<Stmt>,
//! # }
//! # enum Stmt {
//! #     Empty(Token),
//! #     Print(PrintStmt),
//! # }
//! # struct PrintStmt {
//! #     pub keyword: Token,
//! #     pub string: Token,
//! #     pub terminator: Token,
//! # }
//! # impl Parse for Program {
//! #     type Stream = ParseStream;
//! #     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
//! #         let mut statements = vec![];
//! #         while !input.is_empty() {
//! #             statements.push(input.parse()?);
//! #         }
//! #         Ok(Self { statements })
//! #     }
//! # }
//! # impl Parse for Stmt {
//! #     type Stream = ParseStream;
//! #     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
//! #         match input.peek() {
//! #             Some(token) => match token.as_matchable() {
//! #                 (TokenKind::Punct, ";", _) => Ok(Stmt::Empty(input.next().unwrap())),
//! #                 (TokenKind::Keyword, "print", _) => Ok(Stmt::Print(input.parse()?)),
//! #                 (_, _, span) => Err(SpannedError {
//! #                     message: "Expected `;` or `print`".into(),
//! #                     source: input.source(),
//! #                     span: Some(span),
//! #                 }),
//! #             }
//! #             None => Err(SpannedError {
//! #                 message: "Unexpected end of input".into(),
//! #                 source: input.source(),
//! #                 span: None,
//! #             }),
//! #         }
//! #     }
//! # }
//! # impl Parse for PrintStmt {
//! #     type Stream = ParseStream;
//! #     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
//! #         let keyword = input.consume(keyword![print])?;
//! #         let string = input.consume_kind(TokenKind::StringLiteral)?;
//! #         let terminator = input.consume(punct![;])?;
//! #
//! #         Ok(PrintStmt {
//! #             keyword,
//! #             string,
//! #             terminator,
//! #         })
//! #     }
//! # }
//! // ...
//! let input = r#"
//! print "Hello, world!";
//! "#;
//!
//! let mut parser = ParseStream::from(input);
//! let program = parser.parse::<Program>();
//! assert!(program.is_ok());
//!
//! let program = program.unwrap();
//! let stmt = &program.statements[0];
//! assert!(matches!(stmt, Stmt::Print(_)));
//!
//! let Stmt::Print(stmt) = stmt else {
//!     unreachable!();
//! };
//! assert_eq!(stmt.string.lexeme(), "\"Hello, world!\"");
//! assert_eq!(stmt.string.span(), span!(2:7..2:22));
//! # }
//! ```
//! But what does it look like when things don't go so smoothly?
//! ```
//! # #[macro_use]
//! # extern crate gramatika;
//! #
//! # fn main () {
//! # use gramatika::{
//! #     Parse, ParseStreamer, Span, Spanned, SpannedError, Substr, Token as _,
//! # };
//! # #[derive(Debug, Token)]
//! # enum Token {
//! #     #[pattern = "print"]
//! #     Keyword(Substr, Span),
//! #     #[pattern = r#"".*?""#]
//! #     StringLiteral(Substr, Span),
//! #     #[pattern = ";"]
//! #     Punct(Substr, Span),
//! #     #[pattern = r"\S+"]
//! #     Unrecognized(Substr, Span),
//! # }
//! # type Lexer = gramatika::TokenStream<Token>;
//! # type ParseStream = gramatika::ParseStream<Token, Lexer>;
//! #
//! // ...
//!
//! #[derive(Debug)]
//! struct Program {
//!     statements: Vec<Stmt>,
//! }
//!
//! #[derive(Debug)]
//! enum Stmt {
//!     // ...
//! #     Empty(Token),
//! #     Print(PrintStmt),
//! }
//!
//! #[derive(Debug)]
//! struct PrintStmt {
//!     // ...
//! #     pub keyword: Token,
//! #     pub string: Token,
//! #     pub terminator: Token,
//! }
//! # impl Parse for Program {
//! #     type Stream = ParseStream;
//! #     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
//! #         let mut statements = vec![];
//! #         while !input.is_empty() {
//! #             statements.push(input.parse()?);
//! #         }
//! #         Ok(Self { statements })
//! #     }
//! # }
//! # impl Parse for Stmt {
//! #     type Stream = ParseStream;
//! #     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
//! #         match input.peek() {
//! #             Some(token) => match token.as_matchable() {
//! #                 (TokenKind::Punct, ";", _) => Ok(Stmt::Empty(input.next().unwrap())),
//! #                 (TokenKind::Keyword, "print", _) => Ok(Stmt::Print(input.parse()?)),
//! #                 (_, _, span) => Err(SpannedError {
//! #                     message: "Expected `;` or `print`".into(),
//! #                     source: input.source(),
//! #                     span: Some(span),
//! #                 }),
//! #             }
//! #             None => Err(SpannedError {
//! #                 message: "Unexpected end of input".into(),
//! #                 source: input.source(),
//! #                 span: None,
//! #             }),
//! #         }
//! #     }
//! # }
//! # impl Parse for PrintStmt {
//! #     type Stream = ParseStream;
//! #     fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
//! #         let keyword = input.consume(keyword![print])?;
//! #         let string = input.consume_kind(TokenKind::StringLiteral)?;
//! #         let terminator = input.consume(punct![;])?;
//! #
//! #         Ok(PrintStmt {
//! #             keyword,
//! #             string,
//! #             terminator,
//! #         })
//! #     }
//! # }
//!
//! // ...
//!
//! let input = r#"
//! pritn "Hello, world!";
//! "#;
//!
//! let mut parser = ParseStream::from(input);
//! let program = parser.parse::<Program>();
//! assert!(program.is_err());
//!
//! let error = program.unwrap_err();
//! assert_eq!(format!("{error}"), r#"
//! ERROR: Expected `;` or `print`
//!   |
//! 2 | pritn "Hello, world!";
//!   | ^----
//! "#);
//! # }
//! ```
//!
//! ## Next Steps
//!
//! This toy example only scratches the surface. To continue exploring, learn
//! more about `Token` and `Lexer` generation in the [`lexer`] module, and check
//! out the [`parse`] module to learn about all the tools available to you when
//! implementing the `Parse` trait.
//!
//! You can also explore two fully-working, non-trivial example projects at the
//! GitHub repository:
//!
//! * [`examples/lox`] is a parser for the [Lox programming language]
//!   implemented with Gramatika's derive macros.
//!
//! * [`examples/lox_manual_impl`] is a parser that _manually_ implements
//!   Gramatika's traits by hand-writing all of the code that's normally
//!   generated by the derive macros.
//!
//!   This is a great place to start if you're curious about the implementation
//!   details, or if you need to manually implement any of Gramatika's traits to
//!   cover a special use case.
//!
//! [`examples/lox`]: https://github.com/dannymcgee/gramatika/tree/main/examples/lox
//! [`examples/lox_manual_impl`]: https://github.com/dannymcgee/gramatika/tree/main/examples/lox_manual_impl
//! [Lox programming language]: https://craftinginterpreters.com/

pub mod debug;
pub mod error;
pub mod lexer;
pub mod parse;
mod span;

pub use debug::*;
pub use error::*;
pub use lexer::*;
pub use parse::*;
pub use span::*;

pub use arcstr::{self, ArcStr, Substr};

/// A type alias for [`arcstr::ArcStr`] (by default) or [`arcstr::Substr`] (if
/// the `substr-source` feature is enabled).
#[cfg(feature = "substr-source")]
pub type SourceStr = Substr;
/// A type alias for [`arcstr::ArcStr`] (by default) or [`arcstr::Substr`] (if
/// the `substr-source` feature is enabled).
#[cfg(not(feature = "substr-source"))]
pub type SourceStr = ArcStr;

#[cfg(feature = "macros")]
pub use gramatika_macro::*;

#[cfg(feature = "macros")]
#[doc(hidden)]
pub use once_cell;

#[cfg(feature = "macros")]
#[doc(hidden)]
pub use regex_automata;
