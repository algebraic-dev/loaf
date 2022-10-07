use std::rc::Rc;

use loaf_core::{
    term::*,
    types::Level,
    value::{Closure, Env, Stuck, Value},
};
use loaf_span::Span;

pub fn eval_apply(head: Rc<Value>, spine: &Vec<Rc<Value>>) -> Rc<Value> {
    let mut head = head;
    for arg in spine {
        head = match &*head {
            Value::Neutral(stuck, sp) => {
                let mut sp = sp.clone();
                sp.push(arg.clone());
                Rc::new(Value::Neutral(stuck.clone(), sp))
            }
            Value::Lam(name, closure) => apply(closure, Some(name.clone()), arg.clone()),
            _ => unreachable!("Internal Error: Malformed application"),
        }
    }
    head
}

pub fn apply(ctx: &Closure, name: Option<String>, arg: Rc<Value>) -> Rc<Value> {
    eval(&ctx.term, &ctx.env.add(name, arg))
}

pub fn quote_stuck(stuck: &Stuck, base: Level) -> Rc<Term> {
    match stuck {
        Stuck::Rigid(cur_depth) => Rc::new(Term::Var(Var {
            range: Span::Generated,
            index: Level::to_index(base, *cur_depth),
        })),
        Stuck::Fun(name, cur_depth) => Rc::new(Term::Fun(TopLevel {
            range: Span::Generated,
            level: *cur_depth,
            name: name.clone(),
        })),
        Stuck::Data(name, cur_depth) => Rc::new(Term::Data(TopLevel {
            range: Span::Generated,
            level: *cur_depth,
            name: name.clone(),
        })),
        Stuck::Const(name, cur_depth) => Rc::new(Term::Const(TopLevel {
            range: Span::Generated,
            level: *cur_depth,
            name: name.clone(),
        })),
    }
}

pub fn quote(val: &Value, depth: Level) -> Rc<Term> {
    match val {
        Value::Neutral(stuck, spine) => Rc::new(Term::App(App {
            range: Span::Generated,
            head: quote_stuck(stuck, depth),
            spine: spine.iter().map(|x| quote(&x, depth)).collect(),
        })),
        Value::Lam(binder, body) => Rc::new(Term::Lambda(Lambda {
            range: Span::Generated,
            binder: binder.clone(),
            body: quote(&apply(body, Some(binder.clone()), Rc::new(Value::var(depth))), depth.inc()),
        })),
        Value::Pi(binder, ty, body) => Rc::new(Term::Pi(Pi {
            range: Span::Generated,
            binder: binder.clone(),
            typ: quote(ty, depth),
            body: quote(&apply(body, binder.clone(), Rc::new(Value::var(depth))), depth.inc()),
        })),
        Value::Sigma(binder, ty, body) => Rc::new(Term::Sigma(Sigma {
            range: Span::Generated,
            binder: binder.clone(),
            typ: quote(&ty, depth),
            body: quote(&apply(&body, binder.clone(), Rc::new(Value::var(depth))), depth.inc()),
        })),
        Value::Pair(fst, snd) => Rc::new(Term::Pair(Pair {
            range: Span::Generated,
            fst: quote(fst, depth),
            snd: quote(snd, depth),
        })),
        Value::Left(expr) => Rc::new(Term::Left(Left {
            range: Span::Generated,
            term: quote(expr, depth),
        })),
        Value::Right(expr) => Rc::new(Term::Right(Right {
            range: Span::Generated,
            term: quote(expr, depth),
        })),
        Value::Universe => Rc::new(Term::Universe(Universe { range: Span::Generated })),
    }
}

pub fn eval_app(app: &App, env: &Env) -> Rc<Value> {
    let head = eval(&app.head, env);
    let spine = app.spine.iter().map(|a| eval(&a, env)).collect();
    eval_apply(head, &spine)
}

pub fn eval(term: &Term, env: &Env) -> Rc<Value> {
    match term {
        Term::Let(term) => eval(&term.then, &env.add(Some(term.binder.clone()), eval(&term.val, env))),
        Term::Fun(top) => Rc::new(Value::Neutral(Stuck::Fun(top.name.clone(), top.level), vec![])),
        Term::Data(top) => Rc::new(Value::Neutral(Stuck::Data(top.name.clone(), top.level), vec![])),
        Term::Const(top) => Rc::new(Value::Neutral(Stuck::Const(top.name.clone(), top.level), vec![])),
        Term::Universe(_) => Rc::new(Value::Universe),
        Term::Var(term) => env.vars.get(term.index.0).expect("Internal Error: Malformed Term").clone(),
        Term::Pi(term) => Rc::new(Value::Pi(term.binder.clone(), eval(&term.typ, env), Closure::new(env, &term.body))),
        Term::Lambda(term) => Rc::new(Value::Lam(term.binder.clone(), Closure::new(env, &term.body))),
        Term::App(term) => eval_app(term, env),
        Term::Pair(term) => Rc::new(Value::Pair(eval(&term.fst, env), eval(&term.snd, env))),
        Term::Sigma(term) => Rc::new(Value::Sigma(term.binder.clone(), eval(&term.typ, env), Closure::new(env, &term.body))),
        Term::Left(term) => Rc::new(Value::Left(eval(&term.term, env))),
        Term::Right(term) => Rc::new(Value::Right(eval(&term.term, env))),
    }
}
