use crate::matcher::MatcherList;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{Attribute, Generics, Ident, Result, Token, Visibility, WhereClause};

#[derive(Clone)]
pub struct Item {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub name: Ident,
    pub generics: Generics,
    pub eq_token: Token![=],
    pub body: MatcherList,
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Item {
            attrs: Attribute::parse_outer(input)?,
            vis: input.parse()?,
            name: input.parse()?,
            generics: {
                let mut generics: Generics = input.parse()?;
                let where_clause = <Option<WhereClause>>::parse(input)?;
                generics.where_clause = where_clause;
                generics
            },
            eq_token: input.parse()?,
            body: input.parse()?,
        })
    }
}

#[derive(Clone)]
pub struct ItemList {
    pub list: Punctuated<Item, Token![,]>,
}

impl Parse for ItemList {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ItemList {
            list: Punctuated::parse_terminated(input)?,
        })
    }
}
