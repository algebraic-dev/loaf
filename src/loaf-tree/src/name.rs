use loaf_span::Range;

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub range: Range,
}

impl Ident {
    pub fn new(name: String, range: Range) -> Ident {
        Ident { name, range }
    }
}
