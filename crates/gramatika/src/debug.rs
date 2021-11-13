use std::{fmt, marker::PhantomData, rc::Rc, sync::Arc};

pub const INDENT: &str = "  ";

pub trait DebugLisp {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result;
}

// --- Foreign type impls ----------------------------------------------------------------

impl<T: DebugLisp> DebugLisp for &T {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		(*self).fmt(f, indent)
	}
}

impl<T: DebugLisp> DebugLisp for Box<T> {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		self.as_ref().fmt(f, indent)
	}
}

impl<T: DebugLisp> DebugLisp for Rc<T> {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		self.as_ref().fmt(f, indent)
	}
}

impl<T: DebugLisp> DebugLisp for Arc<T> {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		self.as_ref().fmt(f, indent)
	}
}

impl<T: DebugLisp> DebugLisp for Vec<T> {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		DebugLispList::new(f, indent).entries(self).finish()
	}
}

impl<T: DebugLisp> DebugLisp for [T] {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		DebugLispList::new(f, indent).entries(self.iter()).finish()
	}
}

impl<T: DebugLisp> DebugLisp for Rc<[T]> {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		DebugLispList::new(f, indent).entries(self.iter()).finish()
	}
}

impl<T: DebugLisp> DebugLisp for Arc<[T]> {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		DebugLispList::new(f, indent).entries(self.iter()).finish()
	}
}

impl<T: DebugLisp> DebugLisp for Option<T> {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
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
	fn fmt(&self, f: &mut fmt::Formatter, _: usize) -> fmt::Result {
		f.write_str("<PhantomData>")
	}
}

impl DebugLisp for String {
	fn fmt(&self, f: &mut fmt::Formatter, _: usize) -> fmt::Result {
		<Self as fmt::Debug>::fmt(self, f)
	}
}

// --- Helpers ---------------------------------------------------------------------------

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

pub struct DebugLispList<'a, 'b: 'a> {
	fmt: &'a mut fmt::Formatter<'b>,
	result: fmt::Result,
	indent: usize,
	has_entries: bool,
}

impl<'a, 'b: 'a> DebugLispList<'a, 'b> {
	pub fn new(f: &'a mut fmt::Formatter<'b>, indent: usize) -> Self {
		let result = write!(f, "[");

		Self {
			fmt: f,
			result,
			indent,
			has_entries: false,
		}
	}

	pub fn is_pretty(&self) -> bool {
		self.fmt.alternate()
	}

	pub fn entry(&mut self, entry: &dyn DebugLisp) -> &mut Self {
		self.result = self.result.and_then(|_| {
			if self.is_pretty() {
				writeln!(self.fmt)?;
				write!(self.fmt, "{}", INDENT.repeat(self.indent + 1))?;
				entry.fmt(self.fmt, self.indent + 1)?;
				write!(self.fmt, ",")
			} else {
				if self.has_entries {
					write!(self.fmt, ", ")?;
				}
				entry.fmt(self.fmt, self.indent + 1)
			}
		});

		self.has_entries = true;
		self
	}

	pub fn entries<D, I>(&mut self, entries: I) -> &mut Self
	where
		D: DebugLisp,
		I: IntoIterator<Item = D>,
	{
		for entry in entries {
			self.entry(&entry);
		}
		self
	}

	pub fn finish(&mut self) -> fmt::Result {
		self.result = self.result.and_then(|_| {
			if self.is_pretty() {
				writeln!(self.fmt)?;
				write!(self.fmt, "{}]", INDENT.repeat(self.indent))
			} else {
				write!(self.fmt, "]")
			}
		});
		self.result
	}
}
