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
use std::collections::HashSet;


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


// default symbols
const DEFAULT_ASSERTS: [&'static str; 2] = [
    "assert",
    "__builtin_assert",
];

const DEFAULT_UNREACHABLES: [&'static str; 2] = [
    "unreachable",
    "__builtin_unreachable",
];

const DEFAULT_MEMCMPS: [&'static str; 2] = [
    "memcmp",
    "__builtin_memcmp",
];

const DEFAULT_STRCMPS: [&'static str; 2] = [
    "strcmp",
    "__builtin_strcmp",
];



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
        help="Additional prefixes for symbols."
    )]
    prefix: Vec<String>,

    #[structopt(
        short="P", long,
        number_of_values=1,
        help="Additional prefixes for lower/upper case symbol variants."
    )]
    prefix_insensitive: Vec<String>,

    #[structopt(
        long,
        number_of_values=1,
        help="Additional symbols for assert statements."
    )]
    assert: Vec<String>,

    #[structopt(
        long,
        number_of_values=1,
        help="Additional symbols for unreachable statements."
    )]
    unreachable: Vec<String>,

    #[structopt(
        long,
        number_of_values=1,
        help="Additional symbols for memcmp expressions."
    )]
    memcmp: Vec<String>,

    #[structopt(
        long,
        number_of_values=1,
        help="Additional symbols for strcmp expressions."
    )]
    strcmp: Vec<String>,

    #[structopt(
        short, long,
        help="Disable default symbols."
    )]
    no_defaults: bool,

    #[structopt(
        long,
        help="Disable arrow (=>) expressions."
    )]
    no_arrows: bool,

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

    // what symbols are we looking for?
    let mut asserts = HashSet::new();
    let mut unreachables = HashSet::new();
    let mut memcmps = HashSet::new();
    let mut strcmps = HashSet::new();

    // defaults?
    if !opt.no_defaults {
        for assert in DEFAULT_ASSERTS {
            asserts.insert(String::from(assert));
        }
        for unreachable in DEFAULT_UNREACHABLES {
            unreachables.insert(String::from(unreachable));
        }
        for memcmp in DEFAULT_MEMCMPS {
            memcmps.insert(String::from(memcmp));
        }
        for strcmp in DEFAULT_STRCMPS {
            strcmps.insert(String::from(strcmp));
        }
    }

    // prefixes?
    for prefix in opt.prefix.iter().chain(opt.prefix_insensitive.iter()) {
        asserts.insert(format!("{}assert", prefix));
        unreachables.insert(format!("{}unreachable", prefix));
        memcmps.insert(format!("{}memcmp", prefix));
        strcmps.insert(format!("{}strcmp", prefix));
    }

    // upper/lower case prefixes?
    for prefix in opt.prefix_insensitive.iter() {
        asserts.insert(format!("{}assert", prefix.to_ascii_lowercase()));
        unreachables.insert(
            format!("{}unreachable", prefix.to_ascii_lowercase())
        );
        memcmps.insert(format!("{}memcmp", prefix.to_ascii_lowercase()));
        strcmps.insert(format!("{}strcmp", prefix.to_ascii_lowercase()));

        asserts.insert(format!("{}ASSERT", prefix.to_ascii_uppercase()));
        unreachables.insert(
            format!("{}UNREACHABLE", prefix.to_ascii_uppercase())
        );
        memcmps.insert(format!("{}MEMCMP", prefix.to_ascii_uppercase()));
        strcmps.insert(format!("{}STRCMP", prefix.to_ascii_uppercase()));
    }

    // explicit symbols?
    for assert in opt.assert.iter() {
        asserts.insert(String::from(assert));
    }
    for unreachable in opt.unreachable.iter() {
        unreachables.insert(String::from(unreachable));
    }
    for memcmp in opt.memcmp.iter() {
        memcmps.insert(String::from(memcmp));
    }
    for strcmp in opt.strcmp.iter() {
        strcmps.insert(String::from(strcmp));
    }

    // read from file
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
    tree = tree.try_map(|expr| {
        edit(
            expr,
            &asserts,
            &unreachables,
            &memcmps,
            &strcmps,
            !opt.no_arrows
        )
    })?;

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
