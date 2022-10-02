use loaf_span::Range;

#[derive(Debug, Clone)]
pub enum Color {
    Fst,
    Snd,
    Trd,
    Fth,
    Fft,
}

#[derive(Debug, Clone)]
pub enum Style {
    Normal,
    Bright,
    Dim,
}

#[derive(Debug, Clone)]
pub enum Severity {
    Warning,
    Error,
    Information,
}

#[derive(Debug, Clone)]
pub enum Marked {
    Colored(Color, Style, Box<Marked>),
    Normal(String),
    Quoted(String),
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub color: Color,
    pub phrase: Phrase,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct Phrase {
    pub style: Style,
    pub words: Vec<Marked>,
}

pub struct ErrorDescription {
    pub code: u32,
    pub severity: Severity,
    pub filename: String,
    pub title: Phrase,
    pub subtitles: Vec<(Color, Phrase)>,
    pub hints: Vec<Phrase>,
    pub positions: Vec<Annotation>,
}

pub struct ErrorMessage<'a> {
    pub code: &'a str,
    pub desc: &'a ErrorDescription,
}

impl ErrorDescription {
    pub fn to_message<'a>(&'a self, code: &'a str) -> ErrorMessage<'a> {
        ErrorMessage { code, desc: self }
    }
}
