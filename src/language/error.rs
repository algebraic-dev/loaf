use crate::language::position::{Point, Range};
use crate::language::tokens::Token;

#[derive(Debug)]
pub enum SyntaxError {
    UnfinishedString(Point),
    Unexpected(Token, Range),
}
