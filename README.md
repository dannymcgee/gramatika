# Parse Framework

The most uncreatively named parsing toolkit you're like to ever see

### Motivation

Maybe it's my lack of a formal education, but I find parsing grammars (of
whatever flavor) more fiddly and error-prone than just writing parsers by hand.
And what's more, they virtually always require some intermediate step of walking
the generated tree and converting it into something you can actually use. The
cost savings are hard to find when you have to write a parser for the output of
your parser generator.

On the other hand, there are some real pain points associated with hand-written
parsers, mainly in that they require an enormous amount of boilerplate just to
get off the ground.

This project is an attempt to find my Goldilocks zone — your mileage may vary.
Currently, it provides a _lexer_ generator that's dirt simple to use, some
convenience macros, and some barebones parsing primitives (inspired by
[syn](https://crates.io/crates/syn)) — just enough to give you a rolling start
and get out of your way.

It's **extremely** nascent right now, so it's not likely to work for your use
case (hell, it probably doesn't even work as intended!), but I think it's a
nice, ergonomic foundation to build on.
