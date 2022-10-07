use core::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pub line: u32,
    pub column: u32,
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone)]
pub enum Span {
    Localized(Range),
    Generated,
}

pub trait Locatable {
    fn locate(&self) -> Range;
}

impl Position {
    pub fn new(line: u32, column: u32, index: u32) -> Position {
        Position { line, column, index }
    }

    pub fn new_start() -> Position {
        Position::new(0, 0, 0)
    }

    pub fn until_next(&self) -> Range {
        Range {
            start: self.clone(),
            end: self.clone().next(' '),
        }
    }

    pub fn next(&self, chr: char) -> Position {
        match chr {
            '\n' => Position {
                column: 0,
                line: self.line + 1,
                index: self.index + 1,
            },
            _ => Position {
                column: self.column + 1,
                line: self.line,
                index: self.index + 1,
            },
        }
    }
}

impl Range {
    pub fn new(start: Position, end: Position) -> Range {
        Range { start, end }
    }

    pub fn mix(&self, end: Range) -> Range {
        Range {
            start: self.start.clone(),
            end: end.end,
        }
    }
}

impl Span {
    pub fn to_range(&self) -> Option<Range> {
        match self {
            Span::Localized(x) => Some(x.clone()),
            Span::Generated => None
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}
