use std::rc::Rc;

use loaf_core::{
    types::Level,
    value::{Env, Value},
};
use loaf_span::{Position, Range};

#[derive(Clone, Debug)]
pub struct Context {
    pub env: Env,
    pub types: im::hashmap::HashMap<String, (Rc<Value>, Level)>,
    pub pos: Range,
}

impl Context {
    pub fn empty() -> Context {
        Context {
            env: Env {
                vars: im::Vector::new(),
                names: im::Vector::new(),
                depth: Level(0),
            },
            types: im::HashMap::new(),
            pos: Range::new(Position::new(0, 0, 0), Position::new(0, 0, 0)),
        }
    }
    pub fn with_pos(&mut self, pos: Range) {
        self.pos = pos
    }

    pub fn with_ty(&self, name: Option<String>, typ: Rc<Value>) -> Context {
        let mut ctx = if let Some(name) = name.clone() {
            let mut ctx = self.clone();
            ctx.types = ctx.types.clone();
            ctx.types.insert(name.clone(), (typ, ctx.env.depth));
            ctx
        } else {
            self.clone()
        };
        ctx.env.vars.push_front(Rc::new(Value::var(self.env.depth)));
        ctx.env.names.push_front(name);
        ctx.env.depth = ctx.env.depth.inc();
        ctx
    }

    pub fn define(&self, name: String, ty: Rc<Value>, term: Rc<Value>) -> Context {
        let mut ctx = self.clone();
        ctx.types = ctx.types.clone();
        ctx.types.insert(name.clone(), (ty, ctx.env.depth));
        ctx.env.vars.push_front(term);
        ctx.env.names.push_front(Some(name));
        ctx.env.depth = ctx.env.depth.inc();
        ctx
    }
}
