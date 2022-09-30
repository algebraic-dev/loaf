use std::rc::Rc;

use super::{term::Term, var};

pub type Spine = Vec<Rc<Value>>;

pub type Env = im::Vector<(String, Rc<Value>)>;

#[derive(Debug)]
pub enum Value {
    // Some application that is stuck.
    Neutral(Stuck, Spine),

    // Dependent function type
    Pi(String, Rc<Value>, Closure),

    // Lambda
    Lam(String, Closure),

    // Dependent function type
    Sigma(String, Rc<Value>, Closure),

    // Dependent pair
    Pair(Rc<Value>, Rc<Value>),

    // Left
    Left(Rc<Value>),

    // Right
    Right(Rc<Value>),

    // Universe / Type of Types.
    Universe,
}

#[derive(Clone, Debug)]
pub enum Stuck {
    Rigid(var::Level),
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub env: Env,
    pub term: Rc<Term>,
}

impl Value {
    pub fn var(lvl: var::Level) -> Value {
        Value::Neutral(Stuck::Rigid(lvl), vec![])
    }
}
