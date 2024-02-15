#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use structopt::StructOpt;
use anyhow;
use regex::Regex;
use regex::Captures;

use std::fs;
use std::fs::File;
use std::path::PathBuf;
use std::io::Write;
use std::io::BufReader;
use std::io::BufRead;
use std::io::BufWriter;


// The actual parser is over here
mod errors;
mod rc;
mod tokenizer;
use tokenizer::tokenize;
use tokenizer::tokenize_at;
use tokenizer::Token;
mod parser;
use parser::parse;
use parser::Map;
mod edit;
use edit::edit;


// prettyassert code is over here
const PRETTYASSERT: &'static str = include_str!("prettyassert.c");


// CLI arguments
#[derive(Debug, StructOpt)]
#[structopt(
    about="A preprocessor that makes C asserts easier to debug",
    rename_all="kebab",
)]
pub struct Opt {
    input: PathBuf,

    #[structopt(short, long)]
    output: Option<PathBuf>,

    #[structopt(
        short, long,
        number_of_values=1,
        help="Additional symbols for assert statements."
    )]
    assert: Vec<String>,

    #[structopt(
        short, long,
        number_of_values=1,
        help="Additional symbols for unreachable statements."
    )]
    unreachable: Vec<String>,

    #[structopt(
        short="A", long,
        help="Enable arrow (=>) expressions, this is enabled by default."
    )]
    arrow: bool,

    #[structopt(
        short, long,
        help="Disable the default statements/expressions."
    )]
    no_defaults: bool,

    #[structopt(
        short, long,
        default_value="16",
        help="Maximum number of characters to display in strcmp and memcmp.",
    )]
    limit: usize,

    #[structopt(long)]
    dump_tokens: bool,

    #[structopt(long)]
    dump_tree: bool,

    #[structopt(long)]
    dump_modified: bool,
}

// entry point
fn main() -> Result<(), anyhow::Error> {
    let opt = Opt::from_args();

    let input = fs::read_to_string(&opt.input)?;

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
    }

    // parse
    let mut tree = match parse(&opt.input, &tokens) {
        Ok(tree) => tree,
        Err(err) => {
            err.print_context();
            std::process::exit(1);
        }
    };

    if opt.dump_tree {
        println!("{:#?}", tree);
    }

    // edit!
    tree = tree.try_map(|expr| edit(expr, &opt))?;

    if opt.dump_modified {
        println!("{:#?}", tree);
    }

    // flatten and write to file
    if let Some(output) = opt.output {
        let f = File::create(output)?;
        let mut f = BufWriter::new(f);

        // first write out our prettyassert code
        write!(f, "{}",
            Regex::new(r"__PRETTY_ASSERT_(COMMAND|ARGS|LIMIT)")?
                .replace_all(PRETTYASSERT, |m: &Captures| {
                    match &m[0] {
                        "__PRETTY_ASSERT_COMMAND"   => std::env::args().nth(0).unwrap().into(),
                        "__PRETTY_ASSERT_ARGS"      => std::env::args().collect::<Vec<_>>().join(" "),
                        "__PRETTY_ASSERT_LIMIT"     => format!("{}", opt.limit),
                        _ => unreachable!(),
                    }
                })
        )?;

        // add a line pragma to map to original source
        writeln!(f, "#line 1 \"{}\"", opt.input.to_string_lossy())?;

        // now write out our modified tree
        tree.try_visit(|tok: &Token<'_>| {
            // make sure to keep whitespace!
            write!(f, "{}", tok.lws)?;
            write!(f, "{}", tok.tok)?;
            Ok::<_, anyhow::Error>(())
        })?;
    }

    Ok(())
}
