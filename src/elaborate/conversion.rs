use std::rc::Rc;

use crate::core::value::{Closure, Stuck, Value};
use crate::core::var;

fn apply_to(closure: &Closure, name: String, lvl: u32) -> Rc<Value> {
    closure
        .add(name, Rc::new(Value::var(var::Level(lvl))))
        .apply()
}

pub fn conv_spine(lvl: u32, left: &[Rc<Value>], right: &[Rc<Value>]) -> bool {
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

pub fn conv(lvl: u32, left: Rc<Value>, right: Rc<Value>) -> bool {
    use Value::*;
    match (&*left, &*right) {
        (Universe, Universe) => true,
        (Lam(n, b), Lam(n1, b1)) => conv(
            lvl + 1,
            apply_to(b, n.clone(), lvl),
            apply_to(b1, n1.clone(), lvl),
        ),
        (Pi(n, t, b), Pi(n1, t1, b1)) => {
            conv(lvl, t.clone(), t1.clone())
                && conv(
                    lvl + 1,
                    apply_to(b, n.clone(), lvl),
                    apply_to(b1, n1.clone(), lvl),
                )
        }
        (Neutral(Stuck::Rigid(x), sp), Neutral(Stuck::Rigid(x1), sp1)) if x.0 == x1.0 => {
            conv_spine(lvl, sp, sp1)
        }

        (_, _) => false,
    }
}
