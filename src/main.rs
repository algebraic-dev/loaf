mod language;
mod core;
use crate::language::parser::Parser;

use clap::{Parser as P, Subcommand};
use std::fs;

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
            let res = parser.and_then(|mut parser| parser.parse_program());
            match res {
                Ok(res) => println!(
                    "Res {}",
                    res.iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join("\n")
                ),
                Err(err) => println!("Err {:?}", err),
            }
        }
    };
}
