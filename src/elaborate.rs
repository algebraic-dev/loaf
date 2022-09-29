use self::context::Context;
use self::conversion::conv;
use std::rc::Rc;

use crate::core::term::Term;
use crate::core::value::Value;
use crate::core::var;
use crate::language::tree::Expr;

pub mod context;
mod conversion;

#[derive(Debug)]
pub enum CompilerError {
    FunctionType,
    CannotFindVariable(String),
    CantInfer,
    TypeMismatch(Context, Rc<Value>, Rc<Value>),
}

pub fn check(ctx: &mut Context, ast: &Expr, typ: Rc<Value>) -> Result<Rc<Term>, CompilerError> {
    match (ast, &*typ) {
        (
            Expr::Lam {
                range: _,
                binder,
                body,
            },
            Value::Pi(_, typ, pi_body),
        ) => {
            let pi_b = pi_body
                .add(Rc::new(Value::var(var::Level(ctx.depth))))
                .apply();
            let mut ctx = ctx.bind(binder.name.clone(), typ.clone());
            let body = check(&mut ctx, body, pi_b)?;
            Ok(Rc::new(Term::Lambda(binder.name.clone(), body)))
        }
        (ast, _) => {
            let (ast_res, infered_ty) = infer(ctx, ast)?;
            if conv(ctx.depth, infered_ty.clone(), typ.clone()) {
                Ok(ast_res)
            } else {
                Err(CompilerError::TypeMismatch(ctx.clone(), infered_ty, typ))
            }
        }
    }
}

pub fn infer(ctx: &mut Context, ast: &Expr) -> Result<(Rc<Term>, Rc<Value>), CompilerError> {
    match ast {
        Expr::Typ { .. } => Ok(((Rc::new(Term::Universe)), Rc::new(Value::Universe))),
        Expr::App {
            range,
            reason,
            spine,
        } => {
            ctx.set_pos(range);
            let (reason, fun_ty) = infer(ctx, reason)?;
            let mut new_spine = Vec::with_capacity(spine.len());
            let mut res_ty = fun_ty;
            for arg in spine {
                match &*res_ty {
                    Value::Pi(_, typ, body) => {
                        let arg_ty = check(ctx, arg, typ.clone())?;
                        new_spine.push(arg_ty.clone());
                        res_ty = body.add(arg_ty.eval(&ctx.vars)).apply();
                    }
                    _ => return Err(CompilerError::FunctionType),
                }
            }
            Ok((Rc::new(Term::App(reason, new_spine)), res_ty))
        }
        Expr::Pi {
            range,
            binder,
            typ,
            body,
        } => {
            ctx.set_pos(range);
            let typ_res = check(ctx, typ, Rc::new(Value::Universe))?;
            let name = binder.clone().map(|x| x.name).unwrap_or("_".to_string());
            let body_res = check(
                &mut ctx.bind(name.clone(), typ_res.eval(&ctx.vars)),
                body,
                Rc::new(Value::Universe),
            )?;
            Ok((
                Rc::new(Term::Pi(name, typ_res, body_res)),
                Rc::new(Value::Universe),
            ))
        }
        Expr::Var { range: _, name } => match ctx.types.get(&name.name) {
            Some((res, lvl)) => Ok((Rc::new(Term::Var(var::Index(ctx.depth - lvl - 1))), res.clone())),
            None => Err(CompilerError::CannotFindVariable(name.name.clone())),
        },
        Expr::Let {
            range,
            binder,
            typ,
            val,
            body,
        } => {
            ctx.set_pos(range);
            let typ = typ.as_ref().unwrap();
            let typ_res = check(ctx, &typ, Rc::new(Value::Universe))?;
            let typ_eval = typ_res.eval(&ctx.vars);
            let val_res = check(ctx, val, typ_eval.clone())?;
            let val_eval = val_res.eval(&ctx.vars);
            let mut ctx = ctx.define(binder.name.clone(), val_eval, typ_eval);
            let (body, res_ty) = infer(&mut ctx, body)?;
            Ok((
                Rc::new(Term::Let(binder.name.clone(), typ_res, val_res, body)),
                res_ty,
            ))
        }
        Expr::If { .. } => Err(CompilerError::CantInfer),
        Expr::Lam { .. } => Err(CompilerError::CantInfer),
    }
}
