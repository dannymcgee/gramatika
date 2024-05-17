use std::fmt;

use arcstr::Substr;

use crate::{DebugLisp, DebugLispStruct, Span};

#[derive(Clone)]
pub struct SpannedError {
	pub message: String,
	pub source: Substr,
	pub span: Option<Span>,
}

pub type Result<T> = std::result::Result<T, SpannedError>;

impl std::error::Error for SpannedError {}

impl fmt::Display for SpannedError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if f.alternate() {
			write!(f, "{}", self.message)
		} else {
			writeln!(f)?;
			writeln!(f, "ERROR: {}", self.message)?;

			if let Some(span) = self.span {
				let line_num = span.start.line + 1;
				let line_digits = ((line_num as f32).log10() + 1.0) as usize;
				let len = span.end.character - span.start.character;

				let line = self
					.source
					.lines()
					.enumerate()
					.find_map(|(idx, line)| {
						if idx == span.start.line {
							Some(line)
						} else {
							None
						}
					})
					.ok_or(fmt::Error)?;

				let tab_count = line.chars().filter(|c| *c == '\t').count();
				let offset = span.start.character - tab_count + (tab_count * 4);
				let line = line.replace('\t', "    ");

				writeln!(f, "{:>offset$}", "|", offset = line_digits + 2)?;
				writeln!(f, "{} | {}", line_num, line)?;
				write!(f, "{:>offset$} ", "|", offset = line_digits + 2)?;
				write!(f, "{:>offset$}", "^", offset = offset + 1)?;

				if len > 1 {
					write!(f, "{:-<width$}", "-", width = len - 1)?;
				}

				writeln!(f)?;
			}

			Ok(())
		}
	}
}

impl fmt::Debug for SpannedError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.debug_struct("SpannedError")
			.field("message", &self.message)
			.field("source", &self.source)
			.field("span", &self.span)
			.finish()
	}
}

impl DebugLisp for SpannedError {
	fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		DebugLispStruct::new(f, indent, "SpannedError")
			.field("message", &self.message)
			.optional_field(
				"span",
				self.span.as_ref().map(|span| span as &dyn DebugLisp),
			)
			.finish()
	}
}
