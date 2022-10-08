use std::rc::Rc;

use loaf_core::types::Level;
use loaf_core::value::{Closure, Stuck, Value};

use crate::context::{force, Context};
use crate::eval::{apply, quote};
use Value::*;

use crate::context::Hole as Holes;

fn apply_to(closure: &Closure, name: Option<String>, lvl: Level) -> Rc<Value> {
    apply(closure, name, Rc::new(Value::var(lvl)))
}

fn conv_spine(ctx: &mut Context, lvl: Level, left: &[Rc<Value>], right: &[Rc<Value>]) -> bool {
    if left.len() != right.len() {
        false
    } else {
        for i in 0..left.len() {
            if !unify(ctx, lvl, left[i].clone(), right[i].clone()) {
                return false;
            }
        }
        true
    }
}

pub fn conv_stuck(stuck: &Stuck, stuck1: &Stuck) -> bool {
    match (stuck, stuck1) {
        (Stuck::Rigid(x), Stuck::Rigid(y)) => x.0 == y.0,
        (Stuck::Data(_, x), Stuck::Data(_, y)) => x.0 == y.0,
        (Stuck::Fun(_, x), Stuck::Fun(_, y)) => x.0 == y.0,
        (Stuck::Const(_, x), Stuck::Const(_, y)) => x.0 == y.0,
        _ => false
    }
}

pub fn unify(ctx: &mut Context, lvl: Level, left: Rc<Value>, right: Rc<Value>) -> bool {
    let l = force(ctx, left);
    let r = force(ctx, right);
    match (&*l, &*r) {
        (Universe, Universe) => true,
        (Neutral(s, sp), Neutral(f, sp1)) if conv_stuck(s, f) => conv_spine(ctx, lvl, sp, sp1),
        (Pi(n, t, b), Pi(n1, t1, b1)) => unify(ctx, lvl, t.clone(), t1.clone()) && unify(ctx, lvl.inc(), apply_to(b, n.clone(), lvl), apply_to(b1, n1.clone(), lvl)),
        (Lam(n, b), Lam(n1, b1)) => unify(ctx, lvl.inc(), apply_to(b, Some(n.clone()), lvl), apply_to(b1, Some(n1.clone()), lvl)),
        (Sigma(n, t, b), Sigma(n1, t1, b1)) => unify(ctx, lvl, t.clone(), t1.clone()) && unify(ctx, lvl.inc(), apply_to(b, n.clone(), lvl), apply_to(b1, n1.clone(), lvl)),
        (Pair(f, s), Pair(f1, s1)) => unify(ctx, lvl, f.clone(), f1.clone()) && unify(ctx, lvl, s.clone(), s1.clone()),
        (Left(t), Left(t1)) => unify(ctx, lvl, t.clone(), t1.clone()),
        (Right(t), Right(t1)) => unify(ctx, lvl, t.clone(), t1.clone()),
        (Hole(n), Hole(m)) if n == m => true,
        (Hole(l), _) => {
            match &ctx.holes[*l] {
                Holes::Filled(n) => {
                    unify(ctx, lvl, n.clone(), r)},
                Holes::Empty     => {
                    ctx.holes[*l] = Holes::Filled(r);
                    true
                },
            }
        }
        (_, Hole(n)) => {
            match &ctx.holes[*n] {
                Holes::Filled(n) => unify(ctx, lvl, l, n.clone()),
                Holes::Empty     => {
                    println!("Solving {} to {}", n, quote(ctx, l.clone(), lvl));
                    ctx.holes[*n] = Holes::Filled(l.clone());
                    true
                },
            }
        }
        (_, _) => false,
    }
}
