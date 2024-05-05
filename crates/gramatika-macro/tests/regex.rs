#[macro_use]
extern crate gramatika_macro;

fn main() {
	let ident = regex!["[a-zA-Z_][a-zA-Z0-9_]*"];
	let keyword = regex![
		"fn|let|struct|type|var|export",
		"function|private|read(_write)?|storage|uniform|workgroup|write",
		"break|case|continu(e|ing)|default|else(if)?|fallthrough|for|if|loop|return|switch|from",
		"true|false",
		"bitcast|discard|enable|import",
	];
	let ty = regex![
		"array|atomic|bool|[fiu]32|mat[2-4]x[2-4]|ptr|sampler(_comparison)?|vec[2-4]",
		"texture_multisampled_2d",
		"texture_external",
		"texture_depth_(2d|cube)(_array)?",
		"texture_(1d|2d(_array)?|3d|cube(_array)?)",
		"texture_storage_(1d|2d(_array)?|3d)",
	];

	assert!(matches!(ident.find(b"foo--"), Some((0, 3))));
	assert!(matches!(keyword.find(b"continuing--"), Some((0, 10))));
	assert!(matches!(
		ty.find(b"texture_depth_2d_array--"),
		Some((0, 22))
	));
}
