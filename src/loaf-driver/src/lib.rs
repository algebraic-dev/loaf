use errs::{elaboration_err_to_desc, syntax_err_to_description};
use loaf_elaborate::{context::Context, infer};
use loaf_parser::Parser;
use loaf_report::error::ErrorDescription;

pub mod errs;

pub fn typecheck_expr(code: & str) -> Result<(), ErrorDescription> {
    let mut code = code.to_string();
    let mut parser = Parser::init(&mut code).map_err(|err| syntax_err_to_description(&err))?;
    let expr = parser.parse_expr().map_err(|err| syntax_err_to_description(&err))?;
    let _ = infer(&mut Context::empty(), &expr).map_err(|err| elaboration_err_to_desc(&err))?;
    Ok(())
}
