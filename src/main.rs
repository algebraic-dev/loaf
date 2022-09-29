mod core;
mod elaborate;
mod language;

use language::parser::Parser;
use elaborate::{context::Context, CompilerError};
use elaborate::infer;

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
                fs::read_to_string(file).expect("Should have been able to read the file");
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
                            println!("Mismatch between\n expected: '{}'\n      got: '{}'", l.quote(ctx.depth), r.quote(ctx.depth))
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
