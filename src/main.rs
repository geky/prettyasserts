#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use structopt::StructOpt;
use anyhow;

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


// CLI arguments
#[derive(Debug, StructOpt)]
#[structopt(rename_all="kebab")]
struct Opt {
    input: PathBuf,

    #[structopt(short, long)]
    output: Option<PathBuf>,

    #[structopt(long)]
    dump_tokens: bool,

    #[structopt(long)]
    dump_tree: bool,

    #[structopt(long)]
    dump_modified: bool,

    #[structopt(short="t", long)]
    in_toml: bool,
}

// entry point
fn main() -> Result<(), anyhow::Error> {
    let opt = Opt::from_args();

    if opt.in_toml {
        let f = File::open(&opt.input)?;
        let f = BufReader::new(f);
        if let Some(output) = opt.output {
            let mut f_ = File::create(output)?;
            let mut line = 1;
            let mut c_line = 1;
            let mut in_c = false;
            let mut chunk = String::new();
            for line_ in f.lines() {
                let line_ = line_?;
                // switch modes
                if in_c && line_.starts_with("'''") {
                    // tokenize
                    let tokens = match tokenize_at(
                        &opt.input, c_line, 1, &chunk
                    ) {
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
                    tree = tree.try_map(edit)?;

                    if opt.dump_modified {
                        println!("{:#?}", tree);
                    }

                    // flatten and write to file
                    tree.try_visit(|tok: &Token<'_>| {
                        // make sure to keep whitespace!
                        write!(f_, "{}", tok.lws)?;
                        write!(f_, "{}", tok.tok)?;
                        Ok::<_, anyhow::Error>(())
                    })?;

                    drop(tree);
                    in_c = false;
                    chunk.clear();
                    writeln!(f_, "{}", line_)?;
                } else if in_c {
                    chunk.push_str(&line_);
                    chunk.push('\n');

                } else {
                    writeln!(f_, "{}", line_)?;

                    // switch modes
                    if line_.starts_with("code = '''") {
                        in_c = true;
                        c_line = line+1;
                        chunk.clear();
                    }
                }
                line += 1;
            }
        }

    } else {
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
        tree = tree.try_map(edit)?;

        if opt.dump_modified {
            println!("{:#?}", tree);
        }

        // flatten and write to file
        if let Some(output) = opt.output {
            let f = File::create(output)?;
            let mut f = BufWriter::new(f);
            tree.try_visit(|tok: &Token<'_>| {
                // make sure to keep whitespace!
                write!(f, "{}", tok.lws)?;
                write!(f, "{}", tok.tok)?;
                Ok::<_, anyhow::Error>(())
            })?;
        }
    }

    Ok(())
}
