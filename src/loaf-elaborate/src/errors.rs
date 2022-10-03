use std::rc::Rc;

use loaf_core::term::Term;
use loaf_span::Range;

use crate::context::Context;

#[derive(Debug)]
pub enum BinderKind {
    SigmaType,
    PiType,
}

#[derive(Debug)]
pub enum ElaborationError {
    CannotFindVariable(Range, String),
    ExpectedType(Context, BinderKind, Range, Rc<Term>),
    Mismatch(Context, Rc<Term>, Rc<Term>),
    CantInfer(Range),
    Inspection(Context, Rc<Term>),
}
