use std::rc::Rc;

use crate::context::{force, Context, DeclKind};
use crate::conv;
use crate::errors::{BinderKind, ElaborationError};
use crate::eval::{apply, eval, quote};

use loaf_core::term::*;
use loaf_core::types::Level;
use loaf_core::value::{Closure, Value};
use loaf_span::{Locatable, Span};
use loaf_tree::Expr;

pub fn check(ctx: &mut Context, ast: &Expr, typ: Rc<Value>) -> Result<Rc<Term>, ElaborationError> {
    match (ast, &*typ) {
        (Expr::Lam(lambda), Value::Pi(name, typ, body)) => {
            ctx.with_pos(lambda.range.clone());
            let var = Value::var(ctx.env.depth);
            let pi_body = apply(body, name.clone(), Rc::new(var));
            let mut ctx = ctx.with_ty(Some(lambda.binder.name.clone()), typ.clone());
            let body = check(&mut ctx, &lambda.body, pi_body)?;
            Ok(Rc::new(Term::Lambda(Lambda {
                range: Span::Localized(lambda.range.clone()),
                binder: lambda.binder.name.clone(),
                body,
            })))
        }
        (Expr::Hlp(hlp), _) => {
            ctx.with_pos(hlp.range.clone());
            Err(ElaborationError::Inspection(ctx.clone(), quote(&typ, ctx.env.depth)))
        }
        (ast, _) => {
            ctx.with_pos(ast.locate());
            let (ast_res, infered) = infer(ctx, ast)?;
            if conv::conv(ctx, ctx.env.depth, infered.clone(), typ.clone()) {
                Ok(ast_res)
            } else {
                Err(ElaborationError::Mismatch(ctx.clone(), quote(&infered, ctx.env.depth), quote(&typ, ctx.env.depth)))
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
        Expr::Var(vari) => match ctx.types.iter().find(|x| x.0 == vari.name.name) {
            Some((_, (res_ty, lvl))) => Ok((
                Rc::new(Term::Var(Var {
                    range: Span::Localized(vari.range.clone()),
                    index: Level::to_index(ctx.env.depth, *lvl),
                })),
                res_ty.clone(),
            )),
            None => match ctx.decls.get(&vari.name.name) {
                Some(lvl) => {
                    let ty = &ctx.funs_val[lvl.0].0;
                    let name = TopLevel {
                        range: Span::Localized(vari.range.clone()),
                        name: vari.name.name.clone(),
                        level: *lvl,
                    };
                    let expr = match &ctx.funs_val[lvl.0].1 {
                        DeclKind::FunDecl(_) => Term::Fun(name),
                        DeclKind::TypeDecl => Term::Data(name)
                    };
                    Ok((Rc::new(expr), ty.clone()))
                }
                None => Err(ElaborationError::CannotFindVariable(vari.range.clone(), vari.name.name.clone())),
            },
        },
        Expr::Let(letd) => {
            ctx.with_pos(letd.range.clone());
            let (val_res, typ_res, typ_eval) = match &letd.typ {
                Some(typ) => {
                    let typ_res = check(ctx, typ, Rc::new(Value::Universe))?;
                    let typ_eval = eval(&typ_res, &ctx.env);
                    let val_res = check(ctx, &letd.val, typ_eval.clone())?;
                    Ok((val_res, typ_res, typ_eval))
                }
                None => {
                    let (val_res, typ_eval) = infer(ctx, &letd.val)?;
                    Ok((val_res, quote(&typ_eval, ctx.env.depth), typ_eval))
                }
            }?;
            // Probably we need to do it lazyly in the future?
            let val_eval = eval(&val_res, &ctx.env);

            let mut ctx = ctx.define(letd.binder.name.clone(), typ_eval, val_eval);
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
            let body_res = check(&mut ctx.with_ty(ident.clone(), eval(&typ_res, &ctx.env)), &pi.body, Rc::new(Value::Universe))?;
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
                match &*force(ctx, res_ty.clone()) {
                    Value::Pi(n, typ, body) => {
                        let arg_ty = check(ctx, arg, typ.clone())?;
                        new_spine.push(arg_ty.clone());
                        res_ty = apply(body, n.clone(), eval(&arg_ty, &ctx.env));
                    }
                    _ => {
                        ctx.with_pos(app.head.locate());
                        return Err(ElaborationError::ExpectedType(ctx.clone(), BinderKind::PiType, arg.locate(), quote(&res_ty, ctx.env.depth)));
                    }
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
                &mut ctx.with_ty(sigma.binder.clone().map(|x| x.name), eval(&typ_res, &ctx.env)),
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
                Rc::new(Value::Sigma(None, fst_ty, Closure::new(&ctx.env, &quote(&snd_ty, ctx.env.depth)))),
            ))
        }
        Expr::Left(left) => {
            ctx.with_pos(left.range.clone());
            let (expr, typ_res) = infer(ctx, &left.expr)?;
            match &*force(ctx, typ_res.clone()) {
                Value::Sigma(_, typ, _) => Ok((
                    Rc::new(Term::Left(Left {
                        range: Span::Localized(left.range.clone()),
                        term: expr,
                    })),
                    typ.clone(),
                )),
                _ => Err(ElaborationError::ExpectedType(
                    ctx.clone(),
                    BinderKind::SigmaType,
                    left.expr.locate(),
                    quote(&typ_res, ctx.env.depth),
                )),
            }
        }
        Expr::Right(right) => {
            ctx.with_pos(right.range.clone());
            let (expr, typ_res) = infer(ctx, &right.expr)?;
            match &*force(ctx, typ_res.clone()) {
                Value::Sigma(n, _, body) => {
                    let evaluated = Rc::new(Value::Left(eval(&expr, &ctx.env)));
                    Ok((
                        Rc::new(Term::Right(Right {
                            range: Span::Localized(right.range.clone()),
                            term: expr,
                        })),
                        apply(&body.clone(), n.clone(), evaluated),
                    ))
                }
                _ => Err(ElaborationError::ExpectedType(
                    ctx.clone(),
                    BinderKind::SigmaType,
                    right.expr.locate(),
                    quote(&typ_res, ctx.env.depth),
                )),
            }
        }
        Expr::Ann(ann) => {
            ctx.with_pos(ann.range.clone());
            let typ_res = check(ctx, &ann.typ, Rc::new(Value::Universe))?;
            let typ_eval = eval(&typ_res, &ctx.env);
            let expr_res = check(ctx, &ann.expr, typ_eval.clone())?;
            Ok((expr_res, typ_eval))
        }
        Expr::Hlp(x) => Err(ElaborationError::CantInfer(x.range.clone())),
        Expr::Lam(x) => Err(ElaborationError::CantInfer(x.range.clone())),
    }
}
