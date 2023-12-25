use vec1::Vec1;

pub trait Parse {
    type Output;

    fn parse<'a>(&self, source: &'a str) -> Option<(&'a str, Self::Output)>;
}

pub struct Text<'a>(&'a str);

impl<'a> Parse for Text<'a> {
    type Output = &'a str;

    fn parse<'b>(&self, source: &'b str) -> Option<(&'b str, Self::Output)> {
        Some((source.strip_prefix(self.0)?, self.0))
    }
}

pub struct Maybe<T>(T);

impl<T: Parse> Parse for Maybe<T> {
    type Output = Option<T::Output>;

    fn parse<'a>(&self, source: &'a str) -> Option<(&'a str, Self::Output)> {
        match self.0.parse(source) {
            None => Some((source, None)),
            Some((rest, value)) => Some((rest, Some(value))),
        }
    }
}

pub struct ZeroOrMore<T>(T);

impl<T: Parse> Parse for ZeroOrMore<T> {
    type Output = Vec<T::Output>;

    fn parse<'a>(&self, mut source: &'a str) -> Option<(&'a str, Self::Output)> {
        let mut output = Vec::new();

        while let Some((next_source, value)) = self.0.parse(source) {
            source = next_source;
            output.push(value);
        }

        Some((source, output))
    }
}

pub struct OneOrMore<T>(T);

impl<T: Parse> Parse for OneOrMore<T> {
    type Output = Vec1<T::Output>;

    fn parse<'a>(&self, source: &'a str) -> Option<(&'a str, Self::Output)> {
        let (mut source, first_value) = self.0.parse(source)?;
        let mut output = Vec1::new(first_value);

        while let Some((next_source, value)) = self.0.parse(source) {
            source = next_source;
            output.push(value);
        }

        Some((source, output))
    }
}

fn main() {}
