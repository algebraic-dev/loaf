use crate::elaborate::context::Context;
use crate::elaborate::conversion::conv;
use std::rc::Rc;

use crate::core::term::Term;
use crate::core::value::Value;
use crate::core::var;
use crate::language::tree::{Expr, Locate};

use super::errors::CompilerError;

pub fn check(ctx: &mut Context, ast: &Expr, typ: Rc<Value>) -> Result<Rc<Term>, CompilerError> {
    match (ast, &*typ) {
        (Expr::Lam { range, binder, body, }, Value::Pi(n, typ, pi_body)) => {
            ctx.set_pos(range);
            let pi_b = pi_body
                .add(n.clone(), Rc::new(Value::var(var::Level(ctx.depth))))
                .apply();
            let mut ctx = ctx.bind(binder.name.clone(), typ.clone());
            let body = check(&mut ctx, body, pi_b)?;
            Ok(Rc::new(Term::Lambda(binder.name.clone(), body)))
        }
        (Expr::Pair { range, fst, snd }, Value::Sigma(n, typ1, si_body)) => {
            ctx.set_pos(range);
            let fst_res = check(ctx, fst, typ1.clone())?;
            let fst_eval = fst_res.eval(&ctx.vars);
            let si_body_eval = si_body.add(n.clone(), fst_eval).apply();
            let snd_res = check(ctx, snd, si_body_eval)?;
            Ok(Rc::new(Term::Pair(fst_res, snd_res)))
        }
        (Expr::Hlp { .. }, t) => {
            println!("~ Inspection");
            let dep = ctx.depth;
            for (name, (typ, _)) in &ctx.types {
                println!("{:10} : {}", name, typ.quote(dep).pair_with(ctx.clone()))
            }
            println!("\n");
            println!("On: {}", ctx.pos);
            println!("inspection: '{}'", t.quote(dep).pair_with(ctx.clone()));
            Err(CompilerError::FoundHole)
        }
        (ast, _) => {
            ctx.set_pos(&ast.get_range());
            let (ast_res, infered_ty) = infer(ctx, ast)?;
            println!("Where {} : {}", ast, infered_ty.quote(ctx.depth));
            if conv(ctx.depth, infered_ty.clone(), typ.clone()) {
                Ok(ast_res)
            } else {
                Err(CompilerError::TypeMismatch(ctx.clone(), typ, infered_ty))
            }
        }
    }
}

pub fn infer(ctx: &mut Context, ast: &Expr) -> Result<(Rc<Term>, Rc<Value>), CompilerError> {
    match ast {
        Expr::Typ { .. } => Ok(((Rc::new(Term::Universe)), Rc::new(Value::Universe))),
        Expr::Hlp { range } => {
            ctx.set_pos(range);
            println!("Cant infer hole");
            Err(CompilerError::CantInfer)
        }
        Expr::Ann { range, expr, typ } => {
            ctx.set_pos(range);
            let typ_res = check(ctx, typ, Rc::new(Value::Universe))?;
            let typ_eval = typ_res.eval(&ctx.vars);
            let expr_res = check(ctx, expr, typ_eval.clone())?;
            Ok((expr_res, typ_eval))
        }
        Expr::Left { range, expr } => {
            ctx.set_pos(range);
            let (expr, typ_res) = infer(ctx, expr)?;
            match &*typ_res {
                Value::Sigma(_, typ, _) => {
                    Ok((Rc::new(Term::Left(expr)), typ.clone()))
                }
                _ => Err(CompilerError::SigmaType),
            }
        }
        Expr::Right { range, expr } => {
            ctx.set_pos(range);
            let (expr, typ_res) = infer(ctx, expr)?;
            match &*typ_res {
                Value::Sigma(n, _, body) => {
                    let evaluated = Rc::new(Value::Left(expr.eval(&ctx.vars)));
                    Ok((Rc::new(Term::Right(expr)), body.clone().add(n.clone(), evaluated).apply()))
                }
                _ => Err(CompilerError::SigmaType),
            }
        }
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
                    Value::Pi(n, typ, body) => {
                        let arg_ty = check(ctx, arg, typ.clone())?;
                        new_spine.push(arg_ty.clone());
                        res_ty = body.add(n.clone(), arg_ty.eval(&ctx.vars)).apply();
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
            let name = binder
                .clone()
                .map(|x| x.name)
                .unwrap_or_else(|| "_".to_string());
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
            Some((res, lvl)) => Ok((
                Rc::new(Term::Var(var::Index(ctx.depth - lvl - 1))),
                res.clone(),
            )),
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
            match typ {
                Some(typ) => {
                    let typ_res = check(ctx, typ, Rc::new(Value::Universe))?;
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
                None => {
                    let (val_res, typ_eval) = infer(ctx, val)?;
                    let val_eval = val_res.eval(&ctx.vars);
                    let mut ctx = ctx.define(binder.name.clone(), val_eval, typ_eval.clone());
                    let (body, res_ty) = infer(&mut ctx, body)?;
                    Ok((
                        Rc::new(Term::Let(
                            binder.name.clone(),
                            typ_eval.quote(ctx.depth),
                            val_res,
                            body,
                        )),
                        res_ty,
                    ))
                }
            }
        }
        Expr::Sigma { range, binder, typ, body } => {
            ctx.set_pos(range);
            let typ_res = check(ctx, typ, Rc::new(Value::Universe))?;
            let name = binder
                .clone()
                .map(|x| x.name)
                .unwrap_or_else(|| "_".to_string());
            let body_res = check(
                &mut ctx.bind(name.clone(), typ_res.eval(&ctx.vars)),
                body,
                Rc::new(Value::Universe),
            )?;
            Ok((
                Rc::new(Term::Sigma(name, typ_res, body_res)),
                Rc::new(Value::Universe),
            ))
        }
        Expr::If { .. }   => Err(CompilerError::CantInfer),
        Expr::Lam { .. }  => Err(CompilerError::CantInfer),
        Expr::Pair { .. } => Err(CompilerError::CantInfer),
    }
}
