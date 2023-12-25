mod item;
mod matcher;

use crate::item::ItemList;
use matcher::{Clause, Matcher, MatcherAtom};
use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{
    parse,
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Attribute, Expr, Result, Token, Type,
};

struct Header {
    global_attrs: Vec<Attribute>,
    _impl_token: Option<Token![impl]>,
    trait_name: Option<Ident>,
    _comma: Option<Token![,]>,
}

impl Parse for Header {
    fn parse(input: ParseStream) -> Result<Self> {
        let global_attrs = Attribute::parse_outer(input)?;
        let no_attrs = global_attrs.is_empty();

        if input.peek(Token![impl]) {
            Ok(Header {
                global_attrs,
                _impl_token: Some(input.parse()?),
                trait_name: Some(input.parse()?),
                _comma: Some(input.parse()?),
            })
        } else {
            Ok(Header {
                global_attrs,
                _impl_token: None,
                trait_name: None,
                _comma: if no_attrs { None } else { Some(input.parse()?) },
            })
        }
    }
}

struct ParserInput {
    header: Header,
    items: ItemList,
}

impl Parse for ParserInput {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ParserInput {
            header: input.parse()?,
            items: input.parse()?,
        })
    }
}

/// The `parser!()` macro quickly generates parsers for complex streams of text using a simple DSL
/// that compiles to pure Rust. Here's a hello world example:
///
/// ```
/// # use parser_macro::parser;
/// #
/// parser!(
///     #[derive(Debug)],
///     #[derive(Copy, Clone)] Exclamation = '!',
///     HelloWorld  = "Hello" ' '+ "world" Exclamation,
///     HelloWorlds = HelloWorld (' '+ HelloWorld)*,
/// );
///
/// # fn main() {
/// let result = HelloWorlds::parse("Hello world!  Hello   world! Other text");
/// println!("{result:?}");
///
/// // Some(
/// //     (
/// //         HelloWorlds(
/// //             HelloWorld("Hello", [' '], "world", Exclamation('!')),
/// //             [([' ', ' '], HelloWorld("Hello", [' ', ' ', ' '], "world", Exclamation('!')))]
/// //         ),
/// //         " Other text"
/// //     )
/// // )
/// # }
/// ```
///
/// Each of the above parsers will now have a `::parse()` method whose signature is like so:
///
/// ```ignore
/// fn parse(input: &str) -> Option<(Self, &str)>;
/// ```
///
/// That is, the method takes in a single parameter representing the input text to parse, and
/// returns an `Option` containing the parsed result along with the rest of the unparsed text.
///
/// ## Syntax
///
/// The parser macro takes an optional header followed by a set of parsers. The header specifies
/// global attributes, as well as a trait for all generated parsers to implement. Let's look back at
/// our hello world example.
///
/// ### Global Attributes
///
/// ```
/// # use parser_macro::parser;
/// #
/// parser!(
///     #[derive(Debug)],
///     #[derive(Copy, Clone)] Exclamation = '!',
///     HelloWorld  = "Hello" ' '+ "world" Exclamation,
///     HelloWorlds = HelloWorld (' '+ HelloWorld)*,
/// );
/// ```
///
/// Notice the `#[derive(Debug)]` at the top, all alone. That attribute is applied globally, meaning
/// it will be attached to all the generated parser structs. The `Exclamation` parser is also
/// annotated with `#[derive(Copy)]`, although it also implicitly receives `Debug` from the global
/// attribute.
///
/// If you need to opt-out of global attributes for a certain parser, just call `parser!()`
/// separately, like so:
///
/// ```
/// # use parser_macro::parser;
/// #
/// parser!(
///     #[derive(Debug)],
///     #[derive(Copy, Clone)] Exclamation = '!',
///     HelloWorld = "Hello" ' '+ "world" Exclamation,
/// );
///
/// parser!(HelloWorlds = HelloWorld (' '+ HelloWorld)*);
/// ```
///
/// ### Implementing Traits
///
/// Normally, the `parse` methods are inherent `impl`s on the generated parsers. However, you can
/// choose to make them trait `impl`s by writing `impl MyTrait` after the header. The traits are
/// expected to have a single required method called `::parse()` with the same signature as
/// described above. A good use case is for creating generic parsers like `SepBy`, as shown below.
/// And yes, generic parsers are definitely supported.
///
/// ```
/// # use parser_macro::parser;
/// #
/// trait Parse {
///     fn parse(input: &str) -> Option<(Self, &str)>
///     where
///         Self: Sized;
/// }
///
/// parser!(
///     #[derive(Debug)] impl Parse, // Notice the `impl Parse` on this line.
///     SepBy<A: Parse, B: Parse> = A (B A)*,
///     Hello                     = "Hello",
///     Spaces                    = ' '+,
///     Greetings                 = SepBy<Hello, Spaces>,
/// );
/// ```
///
/// The trait passed to `impl Trait` must not be an unsafe trait.
///
/// ### Parser Syntax
///
/// Now let's explain the actual parser syntax. It's quite straightforward, and is based on PEG
/// grammars that have been adapted to fit Rust conventions.
///
/// Each parser is defined as a parser name, an equals sign, and a parser body. The parser name may
/// include (in order:) attributes, visibility modifiers, the name of the parser, generics, and a
/// where clause. Here are a few valid parsers:
///
/// ```
/// # use parser_macro::parser;
/// #
/// trait Parse {
///     fn parse(input: &str) -> Option<(Self, &str)>
///     where
///         Self: Sized;
/// }
///
/// parser!(
///     impl Parse,
///     Hello = "hello", // Basic parser
///     pub World = pub "world", // Visiblity modifier on parser and contents
///     Alpha where i32: Default = 'A'..='Z' / 'a'..='z', // Useless `where` clause
///     #[derive(Clone, Copy, Default)] pub(crate) Greet<T: Parse> where T: Default = "Hello," T '!',
///     // Attributes, visibility, generics, and a where clause, but no public access to contents
/// );
/// ```
///
/// Each parser body is a sequence of matchers. Currently, these matchers are supported:
///
/// - `"literal string"`: matches a literal string and returns a `&'static str`
/// - `char`: matches any character and returns a `char`
/// - `'A'`: matches the uppercase letter A and returns a `char`
/// - `'A'..='Z'`: matches any character from A to Z (inclusive) and returns a `char`
/// - `'A'..='Z' / 'a'..='z' / '0'`: matches A-Z, a-z, or 0 and returns a `char`
/// - `Ident`: matches using `Ident::parse()` and returns an `Ident`
/// - `Ident<T, U>`: matches using `Ident<T, U>::parse()` and returns an `Ident<T, U>`
/// - `(matcher_a matcher_b ...)`: matches a sequence of `matcher_a` followed by `matcher_b`,
///   returning a tuple of their types
///
/// Of special note is the syntax for character matchers, as multiple ranges or characters may be
/// specified separated by slashes. While normal Rust would use pipes `|`, the slashes convey a
/// tighter precedence, and pipes will be used for more general alternative matching in the future.
///
/// A matcher may also have a quantifier attached. If the matcher the quantifier is applied to is
/// called `pattern` and the type it returns is `T`, then these quantifiers are specified:
///
/// - `pattern?`: optionally matches `pattern`, returning `Option<T>`
/// - `pattern*`: matches `pattern` zero or more times, returning `Vec<T>`
/// - `pattern+`: matches `pattern` one or more times, returning `Vec<T>`
///
/// More fine-grained quantifiers, such as `{A,B}`, `{A,}`, and `{,A}` may be added in the future.
///
/// Additionally, top-level parsers may have visiblity qualifiers attached before them.
///
/// ### Matcher Clauses
///
/// A parenthesized expression may have a single optional clause before the final closing
/// parenthesis. Currently, two clauses are supported, shown below.
///
/// ```
/// # use parser_macro::parser;
/// #
/// parser!(
///     Spaces = (' '+ as |spaces| -> usize { spaces.len() }),
///     // The clause above uses an `as` statement followed by a closure. Also notice how the return
///     // type is specified. The return type is required because it must be placed into a field of
///     // the generated parser, and thus must be known without inference.
///     //
///     // The generated struct is defined as `struct Spaces(usize);`.
///
///     Hello = ("hello" as _),
///     // The `as _` clause matches "hello", but discards its result.
///     // The generated struct is defined as `struct Hello;`.
/// );
/// ```
///
/// - `(p1 p2 p3 ... as _)`: match p1, p2, and p3, but don't include the result in the match
/// - `(p1 p2 p3 ... as closure)`: match p1, p2, and p3, and pass them as a tuple to `closure`. If
///   only one item is in the parentheses, it will be passed directly to `closure`.
///
/// ## Planned Functionality
///
/// These items are scheduled to be added sometime in the future:
///
/// - advanced quantifiers such as `{3..=7}` to specify that 3 to 7 items (inclusive) are required
/// - paths used instead of just identifiers for subpatterns
/// - modules of parsers
/// - filtering parser results
/// - parsers that accept multiple inputs
#[proc_macro]
pub fn parser(tokens: TokenStream) -> TokenStream {
    let input: ParserInput = parse(tokens).unwrap();

    let mut output = TokenStream::new();

    let global_attrs = input.header.global_attrs;

    let parse_trait = match input.header.trait_name {
        Some(name) => quote!(#name for),
        None => quote!(),
    };

    for item in input.items.list {
        let name = item.name;
        let (impl_generics, type_generics, where_clause) = item.generics.split_for_impl();

        let expr = item.body.list.iter().map(|x| Expr::from(&x.matcher));

        let attrs = item.attrs;

        let vis = item.vis;

        let properties = item
            .body
            .list
            .iter()
            .map(|x| (Type::from(&x.matcher), &x.vis, &x.matcher))
            .filter(|(_, _, matcher)| {
                !matches!(
                    matcher,
                    Matcher {
                        atom: MatcherAtom::Parenthesized(_, _, Clause::AsIgnored(_, _)),
                        quantifier: _
                    }
                )
            })
            .map(|(ty, vis, _)| quote!(#vis #ty));

        let properties = {
            let props: Vec<_> = properties.collect();

            if props.is_empty() {
                quote!()
            } else {
                quote!((#(#props),*))
            }
        };

        let items = item
            .body
            .list
            .iter()
            .map(|x| (Expr::from(&x.matcher), &x.matcher))
            .enumerate()
            .map(|(index, (expr, matcher))| {
                let ident = format!("__{index}");
                (Ident::new(&ident, expr.span()), expr, matcher)
            });

        let all_indices = items.clone().map(|x| x.0);

        let filtered_indices = items
            .filter(|(_, _, matcher)| {
                !matches!(
                    matcher,
                    Matcher {
                        atom: MatcherAtom::Parenthesized(_, _, Clause::AsIgnored(_, _)),
                        quantifier: _
                    }
                )
            })
            .map(|x| x.0)
            .collect::<Vec<_>>();

        let filtered_indices = if filtered_indices.len() == 0 {
            quote!()
        } else {
            quote!((#(#filtered_indices),*))
        };

        output.extend(TokenStream::from(quote! {
            #(#global_attrs)* #(#attrs)* #vis struct #name #type_generics #properties #where_clause;

            impl #impl_generics #parse_trait #name #type_generics #where_clause {
                fn parse(mut input: &str) -> Option<(Self, &str)> {
                    let (#(#all_indices),*) = (#(#expr?),*);

                    Some((Self #filtered_indices, input))
                }
            }
        }));
    }

    output
}
