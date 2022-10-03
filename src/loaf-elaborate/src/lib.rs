use std::rc::Rc;

use context::Context;
use errors::ElaborationError;
use loaf_core::value::Value;
use loaf_tree::TopLevel;
use expr::check;

pub mod context;
pub mod conv;
pub mod errors;
pub mod expr;

pub fn check_decls(ctx: &mut Context, defs: &Vec<TopLevel>) -> Result<(), ElaborationError> {
  for def in defs {
    match def {
      TopLevel::TypeDecl(_) => todo!(),
      TopLevel::LetDecl(decl) => {
        let ty_checked = check(ctx, &decl.typ, Rc::new(Value::Universe))?;
        let ty_evaluated = ty_checked.eval(&ctx.env);
        ctx.with_toplevel_ty(decl.name.name.clone(), ty_evaluated.clone());
        let expr_checked = check(ctx, &decl.value, ty_evaluated)?;
        ctx.with_toplevel_val(expr_checked);
      }
    }
  }
  Ok(())
}