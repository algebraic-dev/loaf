use core::fmt;
use std::fs;

use clap::{Parser as CParser, Subcommand};
use loaf_driver::typecheck_expr;
use loaf_report::render::RenderConfig;

#[derive(CParser)]
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

fn main() -> Result<(), fmt::Error> {
    let cli = Cli::parse();
    match cli.command {
        Command::Compile { file } => {
            let contents = fs::read_to_string(file.clone()).expect("Should have been able to read the file");
            let result = typecheck_expr(&contents);
            match result {
                Ok(_) => println!("Ok!"),
                Err(err) => {
                    let message = err.to_message(&file, &contents);
                    let config = RenderConfig::ascii();
                    let mut str = String::new();
                    message.render(&config, &mut str)?;
                    println!("{}", str);
                }
            }
        }
    };
    Ok(())
}
