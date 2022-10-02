use std::rc::Rc;

use context::Context;
use errors::{BinderKind, ElaborationError};
use loaf_core::term::*;
use loaf_core::types::Level;
use loaf_core::value::{Closure, Value};
use loaf_span::{Locatable, Span};
use loaf_tree::Expr;

pub mod context;
pub mod conv;
pub mod errors;
pub mod expr;

pub fn check(ctx: &mut Context, ast: &Expr, typ: Rc<Value>) -> Result<Rc<Term>, ElaborationError> {
    match (ast, &*typ) {
        (Expr::Lam(lambda), Value::Pi(name, typ, body)) => {
            ctx.with_pos(lambda.range.clone());
            let var = Value::var(ctx.env.depth);
            let pi_body = body.apply(name.clone(), Rc::new(var));
            let mut ctx = ctx.with_val(Some(lambda.binder.name.clone()), typ.clone());
            let body = check(&mut ctx, &lambda.body, pi_body)?;
            Ok(Rc::new(Term::Lambda(Lambda {
                range: Span::Localized(lambda.range.clone()),
                binder: lambda.binder.name.clone(),
                body,
            })))
        }
        (ast, _) => {
            ctx.with_pos(ast.locate());
            let (ast_res, infered) = infer(ctx, ast)?;
            if conv::conv(ctx.env.depth, infered.clone(), typ.clone()) {
                Ok(ast_res)
            } else {
                Err(ElaborationError::Mismatch(ctx.clone(), infered.quote(ctx.env.depth), typ.quote(ctx.env.depth)))
            }
        }
    }
}

pub fn infer(ctx: &mut Context, ast: &Expr) -> Result<(Rc<Term>, Rc<Value>), ElaborationError> {
    match ast {
        Expr::Typ(typ) => Ok((
            (Rc::new(Term::Universe(Universe {
                range: Span::Localized(typ.range.clone()),
            }))),
            Rc::new(Value::Universe),
        )),
        Expr::Var(vari) => match ctx.types.get(&vari.name.name) {
            Some((res_ty, lvl)) => Ok((
                Rc::new(Term::Var(Var {
                    range: Span::Localized(vari.range.clone()),
                    index: Level::to_index(ctx.env.depth, *lvl),
                })),
                res_ty.clone(),
            )),
            None => Err(ElaborationError::CannotFindVariable(vari.range.clone(), vari.name.name.clone())),
        },
        Expr::Let(letd) => {
            ctx.with_pos(letd.range.clone());
            let (val_res, typ_res, typ_eval) = match &letd.typ {
                Some(typ) => {
                    let typ_res = check(ctx, typ, Rc::new(Value::Universe))?;
                    let typ_eval = typ_res.eval(&ctx.env);
                    let val_res = check(ctx, &letd.val, typ_eval.clone())?;
                    Ok((val_res, typ_res, typ_eval))
                }
                None => {
                    let (val_res, typ_eval) = infer(ctx, &letd.val)?;
                    Ok((val_res, typ_eval.quote(ctx.env.depth), typ_eval))
                }
            }?;
            // Probably we need to do it lazyly in the future?
            let val_eval = val_res.eval(&ctx.env);
            let mut ctx = ctx.define(letd.binder.name.clone(), val_eval, typ_eval);
            let (then_res, then_ty) = infer(&mut ctx, &letd.body)?;
            Ok((
                Rc::new(Term::Let(Let {
                    range: Span::Localized(letd.range.clone()),
                    binder: letd.binder.name.clone(),
                    ty: typ_res,
                    val: val_res,
                    then: then_res,
                })),
                then_ty,
            ))
        }
        Expr::Pi(pi) => {
            ctx.with_pos(pi.range.clone());
            let typ_res = check(ctx, &pi.typ, Rc::new(Value::Universe))?;
            let ident = pi.binder.clone().map(|x| x.name);
            let body_res = check(&mut ctx.with_val(ident.clone(), typ_res.eval(&ctx.env)), &pi.body, Rc::new(Value::Universe))?;
            Ok((
                Rc::new(Term::Pi(Pi {
                    range: Span::Localized(pi.range.clone()),
                    binder: ident,
                    typ: typ_res,
                    body: body_res,
                })),
                Rc::new(Value::Universe),
            ))
        }
        Expr::App(app) => {
            ctx.with_pos(app.range.clone());
            let (head_res, fun_ty) = infer(ctx, &app.head)?;
            let mut new_spine = Vec::with_capacity(app.spine.len());
            let mut res_ty = fun_ty;
            for arg in &app.spine {
                match &*res_ty {
                    Value::Pi(n, typ, body) => {
                        let arg_ty = check(ctx, arg, typ.clone())?;
                        new_spine.push(arg_ty.clone());
                        res_ty = body.apply(n.clone(), arg_ty.eval(&ctx.env));
                    }
                    _ => return Err(ElaborationError::ExpectedType(ctx.clone(), BinderKind::PiType, res_ty.quote(ctx.env.depth))),
                }
            }
            Ok((
                Rc::new(Term::App(App {
                    range: Span::Localized(app.range.clone()),
                    head: head_res,
                    spine: new_spine,
                })),
                res_ty,
            ))
        }
        Expr::Sigma(sigma) => {
            ctx.with_pos(sigma.range.clone());
            let typ_res = check(ctx, &sigma.typ, Rc::new(Value::Universe))?;
            let body_res = check(
                &mut ctx.with_val(sigma.binder.clone().map(|x| x.name), typ_res.eval(&ctx.env)),
                &sigma.body,
                Rc::new(Value::Universe),
            )?;
            Ok((
                Rc::new(Term::Sigma(Sigma {
                    range: Span::Localized(sigma.range.clone()),
                    binder: sigma.binder.clone().map(|x| x.name),
                    typ: typ_res,
                    body: body_res,
                })),
                Rc::new(Value::Universe),
            ))
        }
        Expr::Pair(pair) => {
            ctx.with_pos(pair.range.clone());
            let (fst_elab, fst_ty) = infer(ctx, &pair.fst)?;
            let (snd_elab, snd_ty) = infer(ctx, &pair.snd)?;
            Ok((
                Rc::new(Term::Pair(Pair {
                    range: Span::Localized(pair.range.clone()),
                    fst: fst_elab,
                    snd: snd_elab,
                })),
                Rc::new(Value::Sigma(None, fst_ty, Closure::new(&ctx.env, &snd_ty.quote(ctx.env.depth)))),
            ))
        }
        Expr::Left(left) => {
            ctx.with_pos(left.range.clone());
            let (expr, typ_res) = infer(ctx, &left.expr)?;
            match &*typ_res {
                Value::Sigma(_, typ, _) => Ok((
                    Rc::new(Term::Left(Left {
                        range: Span::Localized(left.range.clone()),
                        term: expr,
                    })),
                    typ.clone(),
                )),
                _ => Err(ElaborationError::ExpectedType(ctx.clone(), BinderKind::SigmaType, typ_res.quote(ctx.env.depth))),
            }
        }
        Expr::Right(right) => {
            ctx.with_pos(right.range.clone());
            let (expr, typ_res) = infer(ctx, &right.expr)?;
            match &*typ_res {
                Value::Sigma(n, _, body) => {
                    let evaluated = Rc::new(Value::Left(expr.eval(&ctx.env)));
                    Ok((
                        Rc::new(Term::Right(Right {
                            range: Span::Localized(right.range.clone()),
                            term: expr,
                        })),
                        body.clone().apply(n.clone(), evaluated),
                    ))
                }
                _ => Err(ElaborationError::ExpectedType(ctx.clone(), BinderKind::SigmaType, typ_res.quote(ctx.env.depth))),
            }
        }
        Expr::Ann(ann) => {
            ctx.with_pos(ann.range.clone());
            let typ_res = check(ctx, &ann.typ, Rc::new(Value::Universe))?;
            let typ_eval = typ_res.eval(&ctx.env);
            let expr_res = check(ctx, &ann.expr, typ_eval.clone())?;
            Ok((expr_res, typ_eval))
        }
        Expr::Hlp(x) => Err(ElaborationError::CantInfer(x.range.clone())),
        Expr::Lam(x) => Err(ElaborationError::CantInfer(x.range.clone())),
    }
}
