use std::rc::Rc;

use context::{Context, DeclKind};
use errors::ElaborationError;
use eval::eval;
use expr::check;
use loaf_core::{types::Level, value::Value, term::{Term, Pi, App}};
use loaf_span::{Span, Range};
use loaf_tree::{DeclRes, TopLevel, TypeDecl};

pub mod context;
pub mod conv;
pub mod decls;
pub mod errors;
pub mod eval;
pub mod expr;

pub fn is_data_constructor(term: &Term, name: String) -> Option<Range> {
  match term {
    Term::Pi(Pi { body, .. }) => is_data_constructor(body, name),
    Term::Data(top) if top.name == name => None,
    Term::App(App { head, .. }) =>  {
      match &*head.clone() {
        Term::Data(top) if top.name == name => None,
        _ => head.locate().to_range(),
      }
    },
    _ => term.locate().to_range(),
  }
}

pub fn is_type_constructor(term: &Term) -> Option<Range> {
    match term {
      Term::Pi(Pi { body, .. }) => is_type_constructor(body),
      Term::Universe(_) => None,
      _ => term.locate().to_range()
    }
}

pub fn check_type_decl(ctx: &mut Context, decl: &TypeDecl) -> Result<(), ElaborationError> {
  let ty_checked = check(ctx, &decl.typ, Rc::new(Value::Universe))?;
  match is_type_constructor(&ty_checked) {
    Some(span) => return Err(ElaborationError::NotATypeConstructor(span)),
    None => (),
  }
  ctx.decls.insert(decl.name.name.clone(), Level(ctx.funs_val.len()));
  ctx.funs_val.push((eval(&ty_checked, &ctx.env), DeclKind::TypeDecl));
  for constr in &decl.constructors {
    let ty_checked = check(ctx, &constr.typ, Rc::new(Value::Universe))?;
    match is_data_constructor(&ty_checked, decl.name.name.clone()) {
      Some(span) => return Err(ElaborationError::NotADataConstructor(span, decl.name.name.clone())),
      None => (),
    }
    ctx.decls.insert(constr.name.name.clone(), Level(ctx.funs_val.len()));
    ctx.funs_val.push((eval(&ty_checked, &ctx.env), DeclKind::DataDecl));
  };
  Ok(())
}

pub fn check_decls(ctx: &mut Context, defs: &Vec<TopLevel>) -> Result<(), ElaborationError> {
    for def in defs {
        match def {
            TopLevel::TypeDecl(decl) => check_type_decl(ctx, decl)?,
            TopLevel::LetDecl(decl) => {
                let ty_checked = check(ctx, &decl.typ, Rc::new(Value::Universe))?;
                let ty_evaluated = eval(&ty_checked, &ctx.env);
                match &decl.value {
                    DeclRes::Value(val) => {
                        let val = check(ctx, val, ty_evaluated.clone())?;
                        ctx.decls.insert(decl.name.name.clone(), Level(ctx.funs_val.len()));
                        ctx.funs_val.push((ty_evaluated, DeclKind::FunDecl(decls::FunDecl::Value(eval(&val, &ctx.env)))));
                    }
                    DeclRes::Pattern(_) => {
                        todo!()
                    }
                }
            }
        }
    }
    Ok(())
}
