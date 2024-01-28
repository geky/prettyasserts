#![allow(dead_code)]

use structopt::StructOpt;
use anyhow;

use std::path::PathBuf;
use std::fs;

// The actual parser is over here
mod errors;
mod tokenizer;
mod parser;
use parser::parse;


// CLI arguments
#[derive(Debug, StructOpt)]
#[structopt(rename_all="kebab")]
struct Opt {
    input: PathBuf,

    #[structopt(short, long, required=true)]
    output: PathBuf,
}

// entry point
fn main() -> Result<(), anyhow::Error> {
    let opt = Opt::from_args();
    let input = fs::read_to_string(&opt.input)?;

    // parse
    let tree = match parse(&opt.input, &input) {
        Ok(tree) => tree,
        Err(err) => {
            err.print_context();
            std::process::exit(1);
        }
    };

    println!("hello! {:?}", &opt);

    Ok(())
}
