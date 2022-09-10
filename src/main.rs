mod language;

use crate::language::lexer::Lexer;
use crate::language::tokens::Token;

use clap::{Parser, Subcommand};
use std::{env, fs};

#[derive(Parser)]
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
      let contents = fs::read_to_string(file).expect("Should have been able to read the file");
      let mut lexer = Lexer::new(&contents);
      while let Ok((tkn, r)) = lexer.lex() {
        match tkn {
          Token::Eof => break,
          other => {
            println!("Res: {} {}", other, r);
          }
        }
      }
    }
  };
}
