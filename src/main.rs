mod core;
mod elaborate;
mod language;

use elaborate::errors::CompilerError;
use language::parser::Parser;
use elaborate::{context::Context};
use elaborate::expr::infer;

use clap::{Parser as P, Subcommand};
use std::{fs};

#[derive(P)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    #[clap(aliases = &["c"])]
    Compile { file: String },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::Compile { file } => {
            let mut contents =
                fs::read_to_string(file.clone()).expect("Should have been able to read the file");
            let parser = Parser::init(&mut contents);
            let res = parser.and_then(|mut parser| parser.parse_expr());
            match res {
                Ok(expr) => {
                    match infer(&mut Context::empty_ctx(), &expr) {
                        Ok((elab, ty)) => {
                            println!("Parsed: {}", expr);
                            println!("Elab: {}", elab);
                            println!("Nf: {}",elab.eval(&im::Vector::new()).quote(0));
                            println!("Ty: {}",ty.quote(0));
                        }
                        Err(CompilerError::TypeMismatch(ctx, l, r)) => {
                            let dep = ctx.depth;
                            for (name, (typ, _)) in &ctx.types {
                                println!("{:10} : {}", name, typ.quote(dep).pair_with(ctx.clone()))
                            }
                            println!("\n");
                            println!("On: {}:{}", file, ctx.pos);
                            println!("Mismatch between\n expected: '{}'\n      got: '{}'", l.quote(dep).pair_with(ctx.clone()), r.quote(dep).pair_with(ctx))
                        }
                        Err(err) => {
                            println!("Err {:?}", err)
                        }
                    }
                },
                Err(err) => println!("Err {:?}", err),
            }
        }
    };
}
