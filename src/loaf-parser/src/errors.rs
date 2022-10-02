use crate::lexer::tokens::Token;
use loaf_span::{Position, Range};

#[derive(Debug)]
pub enum SyntaxError {
    UnfinishedString(Position),
    UnexpectedChar(Position, char),
    UnexpectedToken(Range, Token),
}
