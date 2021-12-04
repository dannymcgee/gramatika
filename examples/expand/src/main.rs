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

	fn (&Expr);
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

	fn (&mut Expr);
});

walker! {
	for SyntaxNode
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{
		if visitor.visit_syntax_node(self) == FlowControl::Continue {
			match self {
				SyntaxNode::Decl(inner) => inner.$walk(visitor),
				SyntaxNode::Stmt(inner) => inner.$walk(visitor),
				SyntaxNode::Expr(inner) => inner.$walk(visitor),
			}
		}
	}
}

walker! {
	for Decl
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{
		if visitor.visit_decl(self) == FlowControl::Continue {
			match self {
				Decl::Struct(inner) => inner.$walk(visitor),
				Decl::Function(inner) => inner.$walk(visitor),
				Decl::Variable(inner) => inner.$walk(visitor),
			}
		}
	}
}

walker! {
	for StructDecl
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{}
}

walker! {
	for FunctionDecl
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{}
}

walker! {
	for VariableDecl
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{}
}

walker! {
	for Stmt
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{
		if visitor.visit_stmt(self) == FlowControl::Continue {
			match self {
				Stmt::If(inner) => inner.$walk(visitor),
				Stmt::For(inner) => inner.$walk(visitor),
				Stmt::Expr(inner) => inner.$walk(visitor),
			}
		}
	}
}

walker! {
	for IfStmt
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{}
}

walker! {
	for ForStmt
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{}
}

walker! {
	for ExprStmt
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{}
}

walker! {
	for Expr
	where
		Walk: fn (&self, visitor: &mut dyn Visitor),
		WalkMut: fn (&mut self, visitor: &mut dyn VisitorMut),
	{
		visitor.visit_expr(self);
	}
}

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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FlowControl {
	Continue,
	Break,
}

impl Default for FlowControl {
	fn default() -> Self {
		FlowControl::Continue
	}
}
