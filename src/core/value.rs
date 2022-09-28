use std::rc::Rc;

use super::{var, term::Term};

pub type Spine = Vec<Rc<Value>>;

pub type Env = im::Vector<Rc<Value>>;

pub enum Value {
  // Some application that is stuck.
  Neutral(Stuck, Spine),

  // Dependent function type
  Pi(String, Rc<Value>, Closure),

  // Lambda
  Lam(String, Closure),

  // Universe / Type of Types.
  Universe
}

#[derive(Clone)]
pub enum Stuck {
  Rigid(var::Level)
}

pub struct Closure {
  pub env: Env,
  pub term: Rc<Term>
}

impl Value {
  pub fn var(lvl: var::Level) -> Value {
    Value::Neutral(Stuck::Rigid(lvl), vec![])
  }
}