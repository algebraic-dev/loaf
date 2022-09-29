use std::fmt;

#[derive(Clone, Debug)]
pub struct Point {
    pub column: u16,
    pub line: u16,
}

#[derive(Clone, Debug)]
pub struct Range {
    pub start: Point,
    pub end: Point,
}

impl Point {
    pub fn new(column: u16, line: u16) -> Point {
        Point { column, line }
    }

    pub fn new_start() -> Point {
        Point::new(1, 1)
    }

    pub fn next(&self, chr: char) -> Point {
        match chr {
            '\n' => Point {
                column: 1,
                line: self.line + 1,
            },
            _ => Point {
                column: self.column + 1,
                line: self.line,
            },
        }
    }
}

impl Range {
    pub fn mix(&self, end: Range) -> Range {
        Range {
            start: self.start.clone(),
            end: end.end,
        }
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.column, self.line)
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}
