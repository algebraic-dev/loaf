use std::{collections::HashMap, rc::Rc};

use loaf_core::{
    types::Level,
    value::{Env, Stuck, Value},
};
use loaf_span::{Position, Range};

use crate::{
    decls::FunDecl,
    eval::eval_apply,
};

#[derive(Debug, Clone)]
pub enum DeclKind {
    FunDecl(FunDecl),
    TypeDecl,
    DataDecl
}

#[derive(Debug, Clone)]
pub struct Context {
    pub env: Env,
    pub types: Vec<(String, (Rc<Value>, Level))>,
    pub decls: HashMap<String, Level>,
    pub funs_val: Vec<(Rc<Value>, DeclKind)>,
    pub pos: Range,
}

pub fn force(ctx: &Context, val: Rc<Value>) -> Rc<Value> {
    match &*val {
        Value::Neutral(Stuck::Fun(_, idx), args) => match &ctx.funs_val[idx.0].1 {
            DeclKind::FunDecl(FunDecl::Value(val)) => eval_apply(val.clone(), args),
            _ => todo!(),
        },
        _ => val,
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
            decls: HashMap::new(),
            funs_val: Vec::new(),
            types: Vec::new(),
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
            ctx.types.push((name, (typ, ctx.env.depth)));
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
        ctx.types.push((name.clone(), (ty, ctx.env.depth)));
        ctx.env.vars.push_front(term);
        ctx.env.names.push_front(Some(name));
        ctx.env.depth = ctx.env.depth.inc();
        ctx
    }
}
