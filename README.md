## Syntax

A parser is defined as a list of segments. Each segment is converted into a
struct or enum with a `Self::parse(source: &str)` method that returns a
`Option<(Self, &str)>`. It should be a `Result`, but I haven't yet put in the
time to make error messages work.

This documents current syntax.

```rust
create_parser!(
    // Literal strings
    HelloWorld = "hello world",
);
```
