//! NOTE: This crate only exists to debug macros.
//! Run `cargo expand -p expand` to check the output of the macros used here.

#[macro_use]
extern crate gramatika;

visitor!(Visitor, Walk for &mut dyn self {
	fn (&SyntaxNode) -> FlowControl;
	fn (&Decl) -> FlowControl;
	fn (&StructDecl);
	fn (&FunctionDecl);
	fn (&VariableDecl);

	fn (&Stmt) -> FlowControl;
	fn (&IfStmt);
	fn (&ForStmt);
	fn (&ExprStmt);

	fn (&Expr) -> FlowControl;
});

visitor!(VisitorMut, WalkMut for &mut dyn self {
	fn (&mut SyntaxNode) -> FlowControl;
	fn (&mut Decl) -> FlowControl;
	fn (&mut StructDecl);
	fn (&mut FunctionDecl);
	fn (&mut VariableDecl);

	fn (&mut Stmt) -> FlowControl;
	fn (&mut IfStmt);
	fn (&mut ForStmt);
	fn (&mut ExprStmt);

	fn (&mut Expr) -> FlowControl;
});

fn main() {}

pub enum SyntaxNode {
	Decl(Decl),
	Stmt(Stmt),
	Expr(Expr),
}

pub enum Decl {
	Struct(StructDecl),
	Function(FunctionDecl),
	Variable(VariableDecl),
}
pub struct StructDecl;
pub struct FunctionDecl;
pub struct VariableDecl;

pub enum Stmt {
	If(IfStmt),
	For(ForStmt),
	Expr(ExprStmt),
}
pub struct IfStmt;
pub struct ForStmt;
pub struct ExprStmt;

pub enum Expr {}

pub enum FlowControl {
	Continue,
	Break,
}

impl Default for FlowControl {
	fn default() -> Self {
		FlowControl::Continue
	}
}
