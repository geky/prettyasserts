#![allow(dead_code)]

use structopt::StructOpt;
use anyhow;

use std::path::PathBuf;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;

// The actual parser is over here
mod errors;
mod tokenizer;
use tokenizer::tokenize;
mod parser;
use parser::parse;


// CLI arguments
fn parse_rc_path(s: &std::ffi::OsStr) -> Rc<PathBuf> {
    Rc::new(PathBuf::from(s))
}

#[derive(Debug, StructOpt)]
#[structopt(rename_all="kebab")]
struct Opt {
    #[structopt(parse(from_os_str=parse_rc_path))]
    input: Rc<PathBuf>,

    #[structopt(short, long)]
    output: Option<PathBuf>,

    #[structopt(long)]
    dump_tokens: bool,

    #[structopt(long)]
    dump_tree: bool,
}

// entry point
fn main() -> Result<(), anyhow::Error> {
    let opt = Opt::from_args();
    let input = fs::read_to_string(opt.input.as_ref())?;

    // tokenize
    let tokens = match tokenize(&opt.input, &input) {
        Ok(tokens) => tokens,
        Err(err) => {
            err.print_context();
            std::process::exit(1);
        }
    };

    if opt.dump_tokens {
        println!("{:#?}", tokens);
        std::process::exit(0);
    }

    // parse
    let tree = match parse(&tokens) {
        Ok(tree) => tree,
        Err(err) => {
            err.print_context();
            std::process::exit(1);
        }
    };

    if opt.dump_tree {
        println!("{:#?}", tree);
        std::process::exit(0);
    }

    // flatten and write to file
    if let Some(output) = opt.output {
        let mut f = File::create(output)?;
        tree.try_visit_tokens(|tok| {
            // make sure to keep whitespace!
            write!(f, "{}", tok.ws)?;
            write!(f, "{}", tok.tok)?;
            Ok::<(), anyhow::Error>(())
        })?;
    }

    Ok(())
}
