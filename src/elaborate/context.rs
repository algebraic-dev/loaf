use std::rc::Rc;

use crate::core::value::{Env, Value};
use crate::core::var;
use crate::language::position::{Point, Range};

#[derive(Clone, Debug)]
pub struct Context {
    pub depth: u32,
    pub vars: Env,
    pub types: im::hashmap::HashMap<String, (Rc<Value>, u32)>,
    pub pos: Range,
}

impl Context {
    pub fn empty_ctx() -> Context {
        Context {
            depth: 0,
            vars: im::Vector::new(),
            types: im::hashmap::HashMap::new(),
            pos: Range {
                start: Point { column: 0, line: 0 },
                end: Point { column: 0, line: 0 },
            },
        }
    }
}

impl Context {
    pub fn set_pos(&mut self, range: &Range) {
        self.pos = range.clone();
    }

    pub fn bind(&self, name: String, ty: Rc<Value>) -> Context {
        let mut vars = self.vars.clone();
        vars.push_front((name.clone(), Rc::new(Value::var(var::Level(self.depth)))));
        Context {
            depth: self.depth + 1,
            vars,
            types: self.types.update(name, (ty, self.depth)),
            pos: self.pos.clone(),
        }
    }

    pub fn define(&self, name: String, val: Rc<Value>, typ: Rc<Value>) -> Context {
        let mut vars = self.vars.clone();
        vars.push_front((name.clone(), val));
        Context {
            depth: self.depth + 1,
            vars,
            types: self.types.update(name, (typ, self.depth)),
            pos: self.pos.clone(),
        }
    }
}
