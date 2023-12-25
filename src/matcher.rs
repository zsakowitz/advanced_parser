use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Paren;
use syn::{
    parenthesized, Expr, ExprClosure, Generics, Ident, LitChar, LitStr, Result, ReturnType, Token,
    Type,
};
use syn::{parse, Visibility};

#[derive(Clone)]
pub enum Clause {
    AsIgnored(Token![as], Token![_]),
    AsClosure(Token![as], ExprClosure),
    None,
}

impl Parse for Clause {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![as]) {
            let as_token: Token![as] = input.parse()?;

            if input.peek(Token![_]) {
                Ok(Clause::AsIgnored(as_token, input.parse()?))
            } else {
                let closure: ExprClosure = input.parse()?;

                Ok(Clause::AsClosure(as_token, closure))
            }
        } else {
            Ok(Clause::None)
        }
    }
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(char);
}

#[derive(Clone)]
pub enum CharMatcher {
    Single(LitChar),
    CharExclusiveRange(LitChar, Token![..], LitChar),
    CharInclusiveRange(LitChar, Token![..=], LitChar),
}

impl Parse for CharMatcher {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(LitChar) {
            let char: LitChar = input.parse()?;

            if input.peek(Token![..=]) {
                Ok(CharMatcher::CharInclusiveRange(
                    char,
                    input.parse()?,
                    input.parse()?,
                ))
            } else if input.peek(Token![..]) {
                Ok(CharMatcher::CharExclusiveRange(
                    char,
                    input.parse()?,
                    input.parse()?,
                ))
            } else {
                Ok(CharMatcher::Single(char))
            }
        } else {
            Err(input.error("expected `char`, a character literal, or a character range literal"))
        }
    }
}

#[derive(Clone)]
pub enum MatcherAtom {
    LitStr(LitStr),
    AnyChar(kw::char),
    Char(Punctuated<CharMatcher, Token![/]>),
    Parser(Ident, Generics),
    AsText(Token![@], Box<Matcher>),
    Parenthesized(Paren, Vec<Matcher>, Clause),
}

impl Parse for MatcherAtom {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![as]) {
            return Err(input.error("MatcherAtoms cannot parse `as`."));
        }

        let lookahead = input.lookahead1();

        if lookahead.peek(Token![@]) {
            Ok(MatcherAtom::AsText(input.parse()?, input.parse()?))
        } else if lookahead.peek(LitStr) {
            input.parse().map(MatcherAtom::LitStr)
        } else if lookahead.peek(kw::char) {
            Ok(MatcherAtom::AnyChar(input.parse()?))
        } else if lookahead.peek(LitChar) {
            Ok(MatcherAtom::Char(Punctuated::parse_separated_nonempty(
                input,
            )?))
        } else if lookahead.peek(Ident) {
            Ok(MatcherAtom::Parser(input.parse()?, input.parse()?))
        } else if lookahead.peek(Paren) {
            let content;
            let paren = parenthesized!(content in input);

            let mut output = Vec::new();

            while let Ok(value) = Matcher::parse(&content) {
                output.push(value);
            }

            let clause: Clause = content.parse()?;

            if !content.is_empty() {
                return Err(content.error("Invalid matchers inside of parenthesized block."));
            }

            Ok(MatcherAtom::Parenthesized(paren, output, clause))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Clone)]
pub enum Quantifier {
    Optional(Token![?]),
    ZeroOrMore(Token![*]),
    OneOrMore(Token![+]),
    ExactlyOne,
}

impl Parse for Quantifier {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![?]) {
            input.parse().map(Quantifier::Optional)
        } else if input.peek(Token![*]) {
            input.parse().map(Quantifier::ZeroOrMore)
        } else if input.peek(Token![+]) {
            input.parse().map(Quantifier::OneOrMore)
        } else {
            Ok(Quantifier::ExactlyOne)
        }
    }
}

#[derive(Clone)]
pub struct Matcher {
    pub atom: MatcherAtom,
    pub quantifier: Quantifier,
}

impl Parse for Matcher {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Matcher {
            atom: input.parse()?,
            quantifier: input.parse()?,
        })
    }
}

#[derive(Clone)]
pub struct TopLevelMatcher {
    pub vis: Visibility,
    pub matcher: Matcher,
}

impl Parse for TopLevelMatcher {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(TopLevelMatcher {
            vis: input.parse()?,
            matcher: input.parse()?,
        })
    }
}

#[derive(Clone)]
pub struct MatcherList {
    pub list: Vec<TopLevelMatcher>,
}

impl Parse for MatcherList {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut list = Vec::new();

        while let Ok(value) = input.parse() {
            list.push(value);
        }

        if list.len() == 0 {
            Err(input.error("Matchers cannot be empty."))
        } else {
            Ok(MatcherList { list })
        }
    }
}

impl From<&MatcherAtom> for Type {
    fn from(value: &MatcherAtom) -> Self {
        match value {
            MatcherAtom::LitStr(_) => parse(quote!(&'static str).into()).unwrap(),

            MatcherAtom::AnyChar(_) => parse(quote!(char).into()).unwrap(),

            MatcherAtom::Char(_) => parse(quote!(char).into()).unwrap(),

            MatcherAtom::Parser(name, generics) => parse(quote!(#name #generics).into()).unwrap(),

            MatcherAtom::AsText(_, _) => parse(quote!(::std::string::String).into()).unwrap(),

            MatcherAtom::Parenthesized(_, value, clause) => match clause {
                Clause::AsIgnored(_, _) => parse(quote!(()).into()).unwrap(),

                Clause::AsClosure(_, closure) => match &closure.output {
                    ReturnType::Default => parse(quote!(()).into()).unwrap(),
                    ReturnType::Type(_, ty) => *ty.clone(),
                },

                Clause::None => {
                    let items = value
                        .iter()
                        .map(|x| (Type::from(x), x))
                        .filter(|(_, value)| {
                            !matches!(
                                value,
                                Matcher {
                                    atom: MatcherAtom::Parenthesized(_, _, Clause::AsIgnored(_, _)),
                                    quantifier: _
                                }
                            )
                        })
                        .map(|(ty, _)| ty);

                    parse(quote!((#(#items),*)).into()).unwrap()
                }
            },
        }
    }
}

impl From<&Matcher> for Type {
    fn from(Matcher { atom, quantifier }: &Matcher) -> Self {
        let atom = Type::from(atom);

        match quantifier {
            Quantifier::Optional(_) => parse(quote!(::core::option::Option<#atom>).into()).unwrap(),

            Quantifier::ZeroOrMore(_) => parse(quote!(::std::vec::Vec<#atom>).into()).unwrap(),

            Quantifier::OneOrMore(_) => parse(quote!(::std::vec::Vec<#atom>).into()).unwrap(),

            Quantifier::ExactlyOne => atom.into(),
        }
    }
}

impl From<&MatcherAtom> for Expr {
    fn from(value: &MatcherAtom) -> Self {
        match value {
            MatcherAtom::LitStr(str) => parse(
                quote! {
                    match input.strip_prefix(#str) {
                        Some(rest) => {
                            input = rest;
                            Some(#str)
                        }

                        None => {
                            None
                        }
                    }
                }
                .into(),
            )
            .unwrap(),

            MatcherAtom::AnyChar(_) => parse(
                quote! {{
                    let mut chars = input.chars();

                    match chars.next() {
                        Some(char) => {
                            input = chars.as_str();
                            Some(char)
                        }

                        None => None
                    }
                }}
                .into(),
            )
            .unwrap(),

            MatcherAtom::Char(matchers) => {
                let pattern = matchers.iter().map(|el| match el {
                    CharMatcher::Single(char) => quote!(#char),
                    CharMatcher::CharExclusiveRange(start, mid, end) => quote!(#start #mid #end),
                    CharMatcher::CharInclusiveRange(start, mid, end) => quote!(#start #mid #end),
                });

                parse(
                    quote! {{
                        let mut chars = input.chars();

                        match chars.next() {
                            Some(char @ (#(#pattern)|*)) => {
                                input = chars.as_str();
                                Some(char)
                            }

                            _ => None,
                        }
                    }}
                    .into(),
                )
                .unwrap()
            }

            MatcherAtom::Parser(name, _) => parse(
                quote! {
                    match #name::parse(input) {
                        Some((value, new_input)) => {
                            input = new_input;
                            Some(value)
                        }

                        None => None
                    }
                }
                .into(),
            )
            .unwrap(),

            MatcherAtom::AsText(_, inner) => {
                let inner = Expr::from(&**inner);

                parse(
                    quote! {{
                        let original_input = input;

                        match #inner {
                            Some(_) => Some(original_input[0..original_input.len() - input.len()].to_owned()),
                            None => None,
                        }
                    }}
                    .into(),
                )
                .unwrap()
            }

            MatcherAtom::Parenthesized(_, value, clause) => {
                let items = value
                    .iter()
                    .map(|x| (Expr::from(x), Type::from(x), x))
                    .enumerate();

                let indices = items.clone().map(|(index, (expr, _, _))| {
                    let name = format!("__{index}");
                    let ident = Ident::new(&name, expr.span());
                    ident
                });

                let values = items.clone().map(|(_, (expr, _, _))| expr);

                let non_empty_indices = items
                    .clone()
                    .filter(|(_, (_, _, value))| {
                        !matches!(
                            value,
                            Matcher {
                                atom: MatcherAtom::Parenthesized(_, _, Clause::AsIgnored(_, _)),
                                quantifier: _
                            }
                        )
                    })
                    .map(|(index, (expr, _, _))| {
                        let name = format!("__{index}");
                        let ident = Ident::new(&name, expr.span());
                        ident
                    });

                let non_empty_types = items
                    .clone()
                    .filter(|(_, (_, _, value))| {
                        !matches!(
                            value,
                            Matcher {
                                atom: MatcherAtom::Parenthesized(_, _, Clause::AsIgnored(_, _)),
                                quantifier: _
                            }
                        )
                    })
                    .map(|(_, (_, ty, _))| ty);

                let intermediate_type = quote!((#(#non_empty_types),*));

                let return_value = match clause {
                    Clause::AsIgnored(_, _) => quote!(Some(())),

                    Clause::AsClosure(_, closure) => {
                        let closure_return_type = match &closure.output {
                            ReturnType::Default => quote!(),
                            ReturnType::Type(_, ty) => quote!(#ty),
                        };

                        quote!({
                            let f: (fn(#intermediate_type) -> #closure_return_type) = #closure;
                            Some(f(value))
                        })
                    }

                    Clause::None => quote!(Some(value)),
                };

                parse(
                    quote! {{
                        let mut modified_input = input;
                        let mut output: ::std::option::Option<#intermediate_type> = None;

                        (|| {
                            let mut input = modified_input;
                            let (#(#indices),*) = (#(#values?),*);
                            output = Some((#(#non_empty_indices),*));
                            modified_input = input;
                            Some(())
                        })();

                        match output {
                            None => None,

                            Some(value) => {
                                input = modified_input;
                                #return_value
                            }
                        }
                    }}
                    .into(),
                )
                .unwrap()
            }
        }
    }
}

impl From<&Matcher> for Expr {
    fn from(Matcher { atom, quantifier }: &Matcher) -> Self {
        let atom = Expr::from(atom);

        match quantifier {
            Quantifier::Optional(_) => parse(quote!(Some(#atom)).into()).unwrap(),

            Quantifier::ZeroOrMore(_) => parse(
                quote!({
                    let mut output = ::std::vec::Vec::new();

                    while let Some(value) = #atom {
                        output.push(value);
                    }

                    Some(output)
                })
                .into(),
            )
            .unwrap(),

            Quantifier::OneOrMore(_) => parse(
                quote!({
                    let mut output = ::std::vec::Vec::new();

                    while let Some(value) = #atom {
                        output.push(value);
                    }

                    if output.len() == 0 {
                        None
                    } else {
                        Some(output)
                    }
                })
                .into(),
            )
            .unwrap(),

            Quantifier::ExactlyOne => atom,
        }
    }
}
