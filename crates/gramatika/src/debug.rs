use std::{fmt, marker::PhantomData};

pub const INDENT: &str = "  ";

pub trait DebugLisp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result;
}

// --- Foreign type impls ----------------------------------------------------------------

impl<T> DebugLisp for Box<T>
where
	T: DebugLisp,
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		self.as_ref().fmt(f, indent)
	}
}

impl<T> DebugLisp for Vec<T>
where
	T: DebugLisp,
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		if self.is_empty() {
			return write!(f, "[]");
		}

		if f.alternate() {
			writeln!(f, "[")?;

			for element in self.iter() {
				write!(f, "{}", INDENT.repeat(indent + 1))?;
				element.fmt(f, indent + 1)?;
				writeln!(f, ",")?;
			}

			write!(f, "{}]", INDENT.repeat(indent))
		} else {
			write!(f, "[")?;

			for (idx, element) in self.iter().enumerate() {
				if idx > 0 && idx != self.len() - 1 {
					write!(f, ", ")?;
				}
				element.fmt(f, indent)?;
			}

			write!(f, "]")
		}
	}
}

impl<T> DebugLisp for Option<T>
where
	T: DebugLisp,
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		match self {
			Some(inner) => {
				write!(f, "Some(")?;
				inner.fmt(f, indent)?;
				write!(f, ")")
			}
			None => write!(f, "None"),
		}
	}
}

impl<T> DebugLisp for PhantomData<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _: usize) -> fmt::Result {
		f.write_str("<PhantomData>")
	}
}

// --- Helper struct ---------------------------------------------------------------------

pub struct DebugLispStruct<'a, 'b: 'a> {
	fmt: &'a mut fmt::Formatter<'b>,
	result: fmt::Result,
	indent: usize,
	has_fields: bool,
}

impl<'a, 'b: 'a> DebugLispStruct<'a, 'b> {
	pub fn new(f: &'a mut fmt::Formatter<'b>, indent: usize, name: &str) -> Self {
		let result = write!(f, "({}", name);

		Self {
			fmt: f,
			result,
			indent,
			has_fields: false,
		}
	}

	pub fn is_pretty(&self) -> bool {
		self.fmt.alternate()
	}

	pub fn field(&mut self, name: &str, value: &dyn DebugLisp) -> &mut Self {
		self.result = self.result.and_then(|_| {
			if self.is_pretty() {
				writeln!(self.fmt)?;
				write!(self.fmt, "{}{}: ", INDENT.repeat(self.indent + 1), name)?;
				value.fmt(self.fmt, self.indent + 1)?;
				write!(self.fmt, ",")
			} else {
				let prefix = if self.has_fields { ", " } else { " " };
				write!(self.fmt, "{}{}: ", prefix, name)?;
				value.fmt(self.fmt, self.indent + 1)
			}
		});

		self.has_fields = true;
		self
	}

	pub fn optional_field(
		&mut self,
		name: &str,
		value: Option<&dyn DebugLisp>,
	) -> &mut Self {
		match value {
			Some(value) => self.field(name, value),
			None => self,
		}
	}

	pub fn finish(&mut self) -> fmt::Result {
		self.result = self.result.and_then(|_| {
			if self.is_pretty() {
				writeln!(self.fmt)?;
				write!(self.fmt, "{})", INDENT.repeat(self.indent))
			} else {
				write!(self.fmt, ")")
			}
		});
		self.result
	}
}
