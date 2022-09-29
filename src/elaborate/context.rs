use std::rc::Rc;

use crate::core::value::{Env, Value};
use crate::core::var;
use crate::language::position::{Range, Point};

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
            pos: Range { start: Point { column: 0, line: 0 }, end: Point { column: 0, line: 0 }  }
        }
    }
}

impl Context {
    pub fn set_pos(&mut self, range: &Range) {
        self.pos = range.clone();
    }

    pub fn bind(&self, str: String, ty: Rc<Value>) -> Context {
        let mut vars = self.vars.clone();
        vars.push_front(Rc::new(Value::var(var::Level(self.depth))));
        Context {
            depth: self.depth + 1,
            vars,
            types: self.types.update(str, (ty, self.depth)),
            pos: self.pos.clone(),
        }
    }

    pub fn define(&self, str: String, val: Rc<Value>, typ: Rc<Value>) -> Context {
        let mut vars = self.vars.clone();
        vars.push_front(val);
        Context {
            depth: self.depth + 1,
            vars,
            types: self.types.update(str, (typ, self.depth)),
            pos: self.pos.clone(),
        }
    }
}
