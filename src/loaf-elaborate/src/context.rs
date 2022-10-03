use std::rc::Rc;

use loaf_core::{
    types::Level,
    value::{Env, Value, Stuck}, term::Term, eval_app,
};
use loaf_span::{Position, Range};

#[derive(Clone, Debug)]
pub struct Context {
    pub env: Env,
    pub top_level_count: Level,
    pub top_level: im::vector::Vector<Rc<Term>>,
    pub top_level_ty: im::hashmap::HashMap<String, (Rc<Value>, Level)>,
    pub types: im::hashmap::HashMap<String, (Rc<Value>, Level)>,
    pub pos: Range,
}

pub fn force(ctx: &Context, val: Rc<Value>) -> Rc<Value> {
    match &*val {
        Value::Neutral(Stuck::Top(_, idx), args) => {
            if let Some(term) = ctx.top_level.get(idx.0) {
                eval_app(term.eval(&ctx.env), args)
            } else {
                panic!("Internal Error: Cannot reduce lol")
            }
        }
        _ => val
    }
}

impl Context {
    pub fn empty() -> Context {
        Context {
            env: Env {
                vars: im::Vector::new(),
                names: im::Vector::new(),
                depth: Level(0),
            },
            top_level: im::Vector::new(),
            top_level_ty: im::HashMap::new(),
            top_level_count: Level(0),
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
            ctx.types.insert(name, (typ, ctx.env.depth));
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

    pub fn with_toplevel_ty(&mut self, name: String, ty: Rc<Value>) {
        self.top_level_ty.insert(name, (ty, self.top_level_count));
        self.top_level_count = self.top_level_count.inc();
    }
    
    pub fn with_toplevel_val(&mut self, val: Rc<Term>) {
        self.top_level.push_back(val);
    }

}
