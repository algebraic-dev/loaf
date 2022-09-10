use crate::language::position::Point;

#[derive(Debug)]
pub enum SyntaxError {
  UnfinishedString(Point)
}