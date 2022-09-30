use std::rc::Rc;

use crate::core::value::Value;

use super::context::Context;

#[derive(Debug)]
pub enum CompilerError {
    FunctionType,
    SigmaType,
    CannotFindVariable(String),
    CantInfer,
    FoundHole,
    TypeMismatch(Context, Rc<Value>, Rc<Value>),
}
