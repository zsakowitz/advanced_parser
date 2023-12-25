use parser_macro::parser;

trait Parse {
    fn parse(input: &str) -> Option<(Self, &str)>
    where
        Self: Sized;
}

parser!(
    #[derive(Clone, Debug)] impl Parse,

    Box<T: Parse> = (T as |x| -> std::boxed::Box<T> { x.into() }),

    Spaces = (' '* as _),

    Number = (@('0'..='9'+) as |digits| -> u32 { digits.parse().unwrap() }),

    MultDiv = (Number (Spaces ('*' / '/') Spaces Number)*
        as |(Number(first), rest)| -> u32 {
            rest.iter().fold(first, |a, (_, op, _, Number(b))| {
                if *op == '*' { a * b } else { a / b }
            })
        }),

    AddSub = (MultDiv (Spaces ('+' / '-') Spaces MultDiv)*
        as |(MultDiv(first), rest)| -> u32 {
            rest.iter().fold(first, |a, (_, op, _, MultDiv(b))| {
                if *op == '+' { a + b } else { a - b }
            })
        }),
);

fn main() {
    let result = AddSub::parse("23 + 57 * 8");
    println!("{result:?}");
}
