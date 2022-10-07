use std::rc::Rc;

use crate::{term::Term, types::Level};

pub type Spine = Vec<Rc<Value>>;

#[derive(Debug, Clone)]
pub struct Env {
    pub vars: im::Vector<Rc<Value>>,
    pub names: im::Vector<Option<String>>,
    pub depth: Level,
}

#[derive(Debug)]
pub enum Value {
    // Some application that is stuck.
    Neutral(Stuck, Spine),

    // Dependent function type
    Pi(Option<String>, Rc<Value>, Closure),

    // Lambda
    Lam(String, Closure),

    // Dependent function type
    Sigma(Option<String>, Rc<Value>, Closure),

    // Dependent pair
    Pair(Rc<Value>, Rc<Value>),

    // Left
    Left(Rc<Value>),

    // Right
    Right(Rc<Value>),

    // Universe / Type of Types.
    Universe,
}

#[derive(Debug, Clone)]
pub enum Stuck {
    Rigid(Level),
    Fun(String, Level),
    Data(String, Level),
    Const(String, Level),
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub env: Env,
    pub term: Rc<Term>,
}

impl Closure {
    pub fn new(env: &Env, term: &Rc<Term>) -> Closure {
        Closure {
            env: env.clone(),
            term: term.clone(),
        }
    }
}

impl Value {
    pub fn var(lvl: Level) -> Value {
        Value::Neutral(Stuck::Rigid(lvl), vec![])
    }
}

impl Env {
    pub fn add(&self, name: Option<String>, term: Rc<Value>) -> Env {
        let mut vars = self.vars.clone();
        let mut names = self.names.clone();
        vars.push_front(term);
        names.push_front(name);

        Env {
            depth: self.depth.inc(),
            vars,
            names,
        }
    }
}
