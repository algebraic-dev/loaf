use std::rc::Rc;

use super::term::Term;
use super::value::{Closure, Env, Stuck, Value};
use super::var;

pub fn eval_app(stuck: Rc<Value>, spine: &[Rc<Value>]) -> Rc<Value> {
    match stuck.as_ref() {
        Value::Neutral(stuck, sp) => Rc::new(Value::Neutral(
            stuck.clone(),
            [sp.as_slice(), spine].concat(),
        )),
        Value::Lam(name, closure) => {
            if spine.is_empty() {
                stuck
            } else {
                eval_app(
                    closure.add(name.clone(), spine[0].clone()).apply(),
                    &spine[1..],
                )
            }
        }
        _ if spine.is_empty() => stuck,
        res => unreachable!("Internal error: Eval app {:?}", res),
    }
}

pub fn eval_spine(env: &Env, spine: &[Rc<Term>]) -> Vec<Rc<Value>> {
    let mut res = Vec::new();
    for term in spine {
        res.push(term.eval(env));
    }
    res
}

impl Closure {
    pub fn apply(&self) -> Rc<Value> {
        self.term.eval(&self.env)
    }

    pub fn add(&self, name: String, arg: Rc<Value>) -> Closure {
        let mut values = self.env.clone();
        values.push_front((name, arg));
        Closure {
            env: values,
            term: self.term.clone(),
        }
    }
}

impl Term {
    pub fn eval(&self, env: &Env) -> Rc<Value> {
        match self {
            Term::App(left, right) => eval_app(left.eval(env), &eval_spine(env, right)),
            Term::Universe => Rc::new(Value::Universe),
            Term::Var(idx) => {
                env.get(idx.0 as usize)
                    .expect("Compiler Error: Malformed term")
                    .clone()
                    .1
            }
            Term::Lambda(name, body) => Rc::new(Value::Lam(
                name.clone(),
                Closure {
                    env: env.clone(),
                    term: body.clone(),
                },
            )),
            Term::Pi(name, typ, body) => Rc::new(Value::Pi(
                name.clone(),
                typ.eval(env),
                Closure {
                    env: env.clone(),
                    term: body.clone(),
                },
            )),
            Term::Let(name, _, val, body) => {
                let mut res = env.clone();
                res.push_front((name.clone(), val.eval(env)));
                body.eval(&res)
            },
            Term::Sigma(name, typ, body) => Rc::new(Value::Sigma(
                name.clone(),
                typ.eval(env),
                Closure {
                    env: env.clone(),
                    term: body.clone(),
                },
            )),
            Term::Pair(fst, snd) => Rc::new(Value::Pair(
                fst.eval(env),
                snd.eval(env)
            )),
            Term::Left(expr) => {
                match &*expr.eval(env) {
                    Value::Pair(l, _) => l.clone(),
                    _ => Rc::new(Value::Left(expr.eval(env)))
                }
            }
            Term::Right(expr) => {
                match &*expr.eval(env) {
                    Value::Pair(_, r) => r.clone(),
                    _ => Rc::new(Value::Right(expr.eval(env)))
                }
            }
        }
    }
}

impl Stuck {
    pub fn quote(&self, lvl: u32) -> Rc<Term> {
        match self {
            Stuck::Rigid(x) => Rc::new(Term::Var(var::Index(lvl - x.0 - 1))),
        }
    }
}

impl Value {
    pub fn quote(&self, lvl: u32) -> Rc<Term> {
        match self {
            Value::Universe => Rc::new(Term::Universe),
            Value::Left(x) => Rc::new(Term::Left(x.quote(lvl))),
            Value::Right(x) => Rc::new(Term::Right(x.quote(lvl))),
            Value::Neutral(stuck, spine) => Rc::new(Term::App(
                stuck.quote(lvl),
                spine.iter().map(|x| x.quote(lvl)).collect(),
            )),
            Value::Pi(name, typ, body) => Rc::new(Term::Pi(
                name.clone(),
                typ.quote(lvl),
                body.add(name.clone(), Rc::new(Value::var(var::Level(lvl))))
                    .apply()
                    .quote(lvl + 1),
            )),
            Value::Lam(name, body) => Rc::new(Term::Lambda(
                name.clone(),
                body.add(name.clone(), Rc::new(Value::var(var::Level(lvl))))
                    .apply()
                    .quote(lvl + 1),
            )),
            Value::Sigma(name, typ, body) => Rc::new(Term::Sigma(
                name.clone(),
                typ.quote(lvl),
                body.add(name.clone(), Rc::new(Value::var(var::Level(lvl))))
                    .apply()
                    .quote(lvl + 1),
            )),
            Value::Pair(fst, snd) => Rc::new(Term::Pair(
                fst.quote(lvl),
                snd.quote(lvl),
            )),
        }
    }
}
