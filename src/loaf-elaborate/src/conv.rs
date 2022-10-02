use std::rc::Rc;

use loaf_core::types::Level;
use loaf_core::value::{Closure, Stuck, Value};

fn apply_to(closure: &Closure, name: Option<String>, lvl: Level) -> Rc<Value> {
    closure.apply(name, Rc::new(Value::var(lvl)))
}

fn conv_spine(lvl: Level, left: &[Rc<Value>], right: &[Rc<Value>]) -> bool {
    if left.len() != right.len() {
        false
    } else {
        for i in 0..left.len() {
            if !conv(lvl, left[i].clone(), right[i].clone()) {
                return false;
            }
        }
        true
    }
}

pub fn conv(lvl: Level, left: Rc<Value>, right: Rc<Value>) -> bool {
    use Value::*;
    match (&*left, &*right) {
        (Universe, Universe) => true,
        (Neutral(Stuck::Rigid(x), sp), Neutral(Stuck::Rigid(x1), sp1)) if x.0 == x1.0 => conv_spine(lvl, sp, sp1),
        (Pi(n, t, b), Pi(n1, t1, b1)) => conv(lvl, t.clone(), t1.clone()) && conv(lvl.inc(), apply_to(b, n.clone(), lvl), apply_to(b1, n1.clone(), lvl)),
        (Lam(n, b), Lam(n1, b1)) => conv(lvl.inc(), apply_to(b, Some(n.clone()), lvl), apply_to(b1, Some(n1.clone()), lvl)),
        (Sigma(n, t, b), Sigma(n1, t1, b1)) => conv(lvl, t.clone(), t1.clone()) && conv(lvl.inc(), apply_to(b, n.clone(), lvl), apply_to(b1, n1.clone(), lvl)),
        (Pair(f, s), Pair(f1, s1)) => conv(lvl, f.clone(), f1.clone()) && conv(lvl, s.clone(), s1.clone()),
        (Left(t), Left(t1)) => conv(lvl, t.clone(), t1.clone()),
        (Right(t), Right(t1)) => conv(lvl, t.clone(), t1.clone()),
        (_, _) => false,
    }
}
