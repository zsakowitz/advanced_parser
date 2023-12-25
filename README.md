## Syntax

A parser is defined as a list of segments. Each segment is converted into a
struct or enum with a `Self::parse(source: &str)` method that returns a
`Option<(Self, &str)>`. It should be a `Result`, but I haven't yet put in the
time to make error messages work.

This documents an example, along with all supported syntax.

```rust
use parser_macro::parser;

trait Parse {
    fn parse(input: &str) -> Option<(Self, &str)>
    where
        Self: Sized;
}

create_parser!(
    // #[derive(Clone, Debug)]  optional attributes which apply to all parsers
    // impl Parser              optional trait name to implement
    #[derive(Clone, Debug)] impl Parse,

    // "hello world"            literal string
    HelloWorld = "hello world",

    // Box<T: Parse>            declares struct name and generics
    // T                        use a nested matcher
    // as ...                   map parser using `...` as a function
    // |x| -> type { value }    take `x` as input, return `value` as type `type`
    //                          types must be explicit
    Box<T: Parse> = (T as |x| -> std::boxed::Box<T> { x.into() }),

    // ' '*                     zero or more space characters
    // as _                     ignore result (makes `Spaces` a ZST)
    //                          without `as _`, `Spaces` is a `Vec<Character>`
    Spaces = (' '* as _),

    // @(...)                   return matched source instead of normal result
    //                          without this, parser would match `Vec<char>`
    // '0'..='9'+               match at least one of the chars `0-9`
    // as ...                   map result using `...` as a function
    Number = (@('0'..='9'+) as |digits| -> u32 { digits.parse().unwrap() }),

    // '*' / '/'                match either `*` or `/`; only works on
    //                          characters and character ranges
    // as ...                   map result using `...` as a function
    MultDiv = (Number (Spaces ('*' / '/') Spaces Number)*
        as |(Number(first), rest)| -> u32 {
            rest.iter().fold(first, |a, (_, op, _, Number(b))| {
                if *op == '*' { a * b } else { a / b }
            })
        }),

    // same as `MultDiv`
    AddSub = (MultDiv (Spaces ('+' / '-') Spaces MultDiv)*
        as |(MultDiv(first), rest)| -> u32 {
            rest.iter().fold(first, |a, (_, op, _, MultDiv(b))| {
                if *op == '+' { a + b } else { a - b }
            })
        }),
);
```
