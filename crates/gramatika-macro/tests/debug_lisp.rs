#[macro_use]
extern crate gramatika;

use gramatika::{Span, Substr};

/// Expected output:
///
/// ```
/// impl ::gramatika::DebugLisp for Token {
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
/// impl ::gramatika::DebugLisp for Expr {
///     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
///         write!(f, "{}::", "Expr")?;
///
///         match self {
///             Expr::Binary(inner) => {
///                 write!(f, "{}(", "BinaryExpr")?;
///                 <BinaryExpr as ::gramatika::DebugLisp>::fmt(&inner, f, indent)
///             }
///             Expr::Unary(inner) => {
///                 write!(f, "{}(", "UnaryExpr")?;
///                 <UnaryExpr as ::gramatika::DebugLisp>::fmt(&inner, f, indent)
///             }
///             Expr::Primary(inner) => {
///                 write!(f, "{}(", "PrimaryExpr")?;
///                 <PrimaryExpr as ::gramatika::DebugLisp>::fmt(&inner, f, indent)
///             }
///         }?;
///
///         write!(f, ")")
///     }
/// }
///
/// impl ::gramatika::DebugLisp for BinaryExpr {
///     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
///         ::gramatika::DebugLispStruct::new(f, indent, "BinaryExpr")
///             .field("lhs", &self.lhs)
///             .field("op", &self.op)
///             .field("rhs", &self.rhs)
///             .finish()
///     }
/// }
///
/// impl ::gramatika::DebugLisp for UnaryExpr {
///     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
///         ::gramatika::DebugLispStruct::new(f, indent, "UnaryExpr")
///             .field("op", &self.op)
///             .field("rhs", &self.rhs)
///             .finish()
///     }
/// }
///
/// impl ::gramatika::DebugLisp for PrimaryExpr {
///     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>, indent: usize) -> ::std::fmt::Result {
///         ::gramatika::DebugLispStruct::new(f, indent, "PrimaryExpr")
///             .field("token", &self.token)
///             .finish()
///     }
/// }
///
/// impl ::core::fmt::Debug for Program {
///     fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
///         write!(f, "{}: ", "Program")?;
///         ::gramatika::DebugLisp::fmt(&self.exprs, f, 0)
///     }
/// }
/// ```

// ...

#[derive(Debug, Token, DebugLispToken)]
pub enum Token {
	Keyword(Substr, Span),
	Ident(Substr, Span),
	Punct(Substr, Span),
	Operator(Substr, Span),
	Literal(Substr, Span),
}

#[derive(DebugLisp)]
pub enum Expr {
	Binary(BinaryExpr),
	Unary(UnaryExpr),
	Primary(PrimaryExpr),
}

#[derive(DebugLisp)]
pub struct BinaryExpr {
	pub lhs: Box<Expr>,
	pub op: Token,
	pub rhs: Box<Expr>,
}

#[derive(DebugLisp)]
pub struct UnaryExpr {
	pub op: Token,
	pub rhs: Box<Expr>,
}

#[derive(DebugLisp)]
pub struct PrimaryExpr {
	pub token: Token,
	pub maybe: Option<Token>,
}

#[derive(DebugLisp)]
pub struct Program {
	pub exprs: Vec<Expr>,
}

fn main() {
	let ast = Program {
		exprs: vec![Expr::Binary(BinaryExpr {
			lhs: Box::new(Expr::Primary(PrimaryExpr {
				token: Token::Literal("3".into(), span![1:1..1:2]),
				maybe: None,
			})),
			op: Token::Operator("+".into(), span![1:3..1:4]),
			rhs: Box::new(Expr::Unary(UnaryExpr {
				op: Token::Operator("-".into(), span![1:5..1:6]),
				rhs: Box::new(Expr::Primary(PrimaryExpr {
					token: Token::Literal("2".into(), span![1:6..1:7]),
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
        token: `3` (Literal (1:1..1:2)),
      )),
      op: `+` (Operator (1:3..1:4)),
      rhs: (Expr::Unary (UnaryExpr
        op: `-` (Operator (1:5..1:6)),
        rhs: (Expr::Primary (PrimaryExpr
          token: `2` (Literal (1:6..1:7)),
        )),
      )),
    )),
  ],
)
"#;

	assert_eq!(output, &expected[1..expected.len() - 1]);
}
