#[macro_use]
extern crate gramatika;

use gramatika::Span;

/// Expected output:
///
/// ```
/// impl<'a> ::gramatika::DebugLisp for Token<'a> {
///     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, _: usize) -> ::std::fmt::Result {
///         write!(
///             f,
///             "`{}` ({:?} [{:?}])",
///             <Self as ::gramatika::Token>::lexeme(self),
///             <Self as ::gramatika::Token>::kind(self),
///             <Self as ::gramatika::Token>::span(self)
///         )
///     }
/// }
///
/// impl<'a> ::gramatika::DebugLisp for Expr<'a> {
///     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
///         write!(f, "{}::", "Expr")?;
///
///         match self {
///             Expr::Binary(inner) => {
///                 write!(f, "{}(", "BinaryExpr")?;
///                 <BinaryExpr<'a> as ::gramatika::DebugLisp>::fmt(&inner, f, indent)
///             }
///             Expr::Unary(inner) => {
///                 write!(f, "{}(", "UnaryExpr")?;
///                 <UnaryExpr<'a> as ::gramatika::DebugLisp>::fmt(&inner, f, indent)
///             }
///             Expr::Primary(inner) => {
///                 write!(f, "{}(", "PrimaryExpr")?;
///                 <PrimaryExpr<'a> as ::gramatika::DebugLisp>::fmt(&inner, f, indent)
///             }
///         }?;
///
///         write!(f, ")")
///     }
/// }
///
/// impl<'a> ::gramatika::DebugLisp for BinaryExpr<'a> {
///     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
///         ::gramatika::DebugLispStruct::new(f, indent, "BinaryExpr")
///             .field("lhs", &self.lhs)
///             .field("op", &self.op)
///             .field("rhs", &self.rhs)
///             .finish()
///     }
/// }
///
/// impl<'a> ::gramatika::DebugLisp for UnaryExpr<'a> {
///     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
///         ::gramatika::DebugLispStruct::new(f, indent, "UnaryExpr")
///             .field("op", &self.op)
///             .field("rhs", &self.rhs)
///             .finish()
///     }
/// }
///
/// impl<'a> ::gramatika::DebugLisp for PrimaryExpr<'a> {
///     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
///         ::gramatika::DebugLispStruct::new(f, indent, "PrimaryExpr")
///             .field("token", &self.token)
///             .finish()
///     }
/// }
///
/// impl<'a> ::core::fmt::Debug for Program<'a> {
///     fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
///         write!(f, "{}: ", "Program")?;
///         ::gramatika::DebugLisp::fmt(&self.exprs, f, 0)
///     }
/// }
/// ```

// ...

#[derive(Debug, Token, DebugLispToken)]
pub enum Token<'a> {
	Keyword(&'a str, Span),
	Ident(&'a str, Span),
	Punct(&'a str, Span),
	Operator(&'a str, Span),
	Literal(&'a str, Span),
}

#[derive(DebugLisp)]
pub enum Expr<'a> {
	Binary(BinaryExpr<'a>),
	Unary(UnaryExpr<'a>),
	Primary(PrimaryExpr<'a>),
}

#[derive(DebugLisp)]
pub struct BinaryExpr<'a> {
	pub lhs: Box<Expr<'a>>,
	pub op: Token<'a>,
	pub rhs: Box<Expr<'a>>,
}

#[derive(DebugLisp)]
pub struct UnaryExpr<'a> {
	pub op: Token<'a>,
	pub rhs: Box<Expr<'a>>,
}

#[derive(DebugLisp)]
pub struct PrimaryExpr<'a> {
	pub token: Token<'a>,
	pub maybe: Option<Token<'a>>,
}

#[derive(DebugLisp)]
pub struct Program<'a> {
	pub exprs: Vec<Expr<'a>>,
}

fn main() {
	let ast = Program {
		exprs: vec![Expr::Binary(BinaryExpr {
			lhs: Box::new(Expr::Primary(PrimaryExpr {
				token: Token::Literal("3", span![0:0...0:1]),
				maybe: None,
			})),
			op: Token::Operator("+", span![0:2...0:3]),
			rhs: Box::new(Expr::Unary(UnaryExpr {
				op: Token::Operator("-", span![0:4...0:5]),
				rhs: Box::new(Expr::Primary(PrimaryExpr {
					token: Token::Literal("2", span![0:5...0:6]),
					maybe: None,
				})),
			})),
		})],
	};

	let output = format!("{:#?}", ast);
	let expected = r#"
(Program
  exprs: [
    (Expr::Binary (BinaryExpr
      lhs: (Expr::Primary (PrimaryExpr
        token: `3` (Literal (1:1...1:2)),
      )),
      op: `+` (Operator (1:3...1:4)),
      rhs: (Expr::Unary (UnaryExpr
        op: `-` (Operator (1:5...1:6)),
        rhs: (Expr::Primary (PrimaryExpr
          token: `2` (Literal (1:6...1:7)),
        )),
      )),
    )),
  ],
)
"#;

	assert_eq!(output, &expected[1..expected.len() - 1]);
}
