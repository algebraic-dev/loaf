use std::rc::Rc;

use loaf_core::{value::Value, term::Term};

#[derive(Debug)]
pub enum CaseTree {
    Term(Term),
    Intro(String, Box<CaseTree>),
    Split(Vec<(String, Vec<String>, CaseTree)>),
}

#[derive(Clone, Debug)]
pub enum FunDecl {
    Value(Rc<Value>),
    Patterns(Rc<CaseTree>),
}
