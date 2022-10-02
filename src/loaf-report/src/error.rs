use std::vec;

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
    pub title: Phrase,
    pub subtitles: Vec<(Color, Phrase)>,
    pub hints: Vec<Phrase>,
    pub positions: Vec<Annotation>,
    pub canon_pos: Range,
}

pub struct ErrorMessage<'a> {
    pub code: &'a str,
    pub filename: &'a str,
    pub desc: &'a ErrorDescription,
}

impl Phrase {
    pub fn new(style: Style, words: Vec<Marked>) -> Phrase {
        Phrase { style, words }
    }

    pub fn bright(text: &str) -> Phrase {
        Phrase {
            style: Style::Bright,
            words: vec![Marked::Normal(text.to_string())],
        }
    }
}

impl Annotation {
    pub fn new(color: Color, phrase: Phrase, range: Range) -> Annotation {
        Annotation { color, phrase, range }
    }

    pub fn bright(color: Color, phrase: &str, range: Range) -> Annotation {
        Annotation::new(color, Phrase::bright(phrase), range)
    }
}

impl ErrorDescription {
    pub fn new(code: u32, range: Range) -> ErrorDescription {
        ErrorDescription {
            code,
            severity: Severity::Error,
            title: Phrase {
                style: Style::Normal,
                words: vec![],
            },
            subtitles: vec![],
            hints: vec![],
            positions: vec![],
            canon_pos: range,
        }
    }

    pub fn set_title(&mut self, phrase: Phrase) {
        self.title = phrase;
    }

    pub fn add_pos(&mut self, ann: Annotation) {
        self.positions.push(ann);
    }

    pub fn add_subtitle(&mut self, color: Color, phrase: Phrase) {
        self.subtitles.push((color, phrase))
    }

    pub fn to_message<'a>(&'a self, filename: &'a str, code: &'a str) -> ErrorMessage<'a> {
        ErrorMessage { code, filename, desc: self }
    }
}
