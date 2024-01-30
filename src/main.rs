#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use structopt::StructOpt;
use anyhow;
use either::{Left, Right};

use std::path::PathBuf;
use std::fs;
use std::fs::File;
use std::rc::Rc;
use std::ops::Deref;
use std::borrow::Cow;
use std::io::BufReader;
use std::io::BufRead;
use std::io::BufWriter;
use std::io::Write;


// The actual parser is over here
mod errors;
mod tokenizer;
use tokenizer::tokenize;
use tokenizer::tokenize_at;
use tokenizer::Token;
use tokenizer::Tt;
mod pool;
use pool::Pool;
use pool::Swim;
mod parser;
use parser::parse;
use parser::Expr;


fn tok<'a>(s: &'a str) -> Token<'a> {
    Token::new(Tt::Sym, s)
}

fn sym<'a>(s: &'a str) -> Expr<'a> {
    Expr::Sym(tok(s))
}

fn squiggle<'a>(o: &mut Pool<'a>, exprs: &[Expr<'a>]) -> Expr<'a> {
    let mut list = vec![];
    for (i, expr) in exprs.iter().enumerate() {
        list.push((
            Some(expr.clone()),
            if i < exprs.len()-1 {
                Some(tok(";"))
            } else {
                None
            }
        ))
    }

    Expr::Squiggle(
        tok(""),
        list.swim(o),
        tok(""),
    )
}


// modify the tree
fn modify<'a>(o: &mut Pool<'a>, expr: Expr<'a>) -> Result<Expr<'a>, anyhow::Error> {
    if let
        Expr::Binary(
            Expr::Call(Expr::Sym(sym_@Token{tok: "lfsr_rbyd_get", ..}), lp, args, rp),
            arrow@Token{tt: Tt::BigArrow, ..},
            rh
        )
    = expr {
        let rh = match rh {
            rh@Expr::Sym(sym_) if sym_.tok.starts_with("LFS_ERR_") => Right(rh),
            rh => Left(rh),
        };

        let mut list_ = vec![];
        list_.push(Expr::Binary(
            Expr::Call(
                sym("lfsr_rbyd_lookup").ws(o, sym_.ws).swim(o),
                *lp,
                vec![
                    args[0].clone(),
                    args[1].clone(),
                    args[2].clone(),
                    args[3].clone(),
                    (Some(sym("&data").ws(o, " ")), None)
                ].swim(o),
                *rp,
            ).swim(o),
            match rh {
                Left(_) => tok("=>").ws(" "),
                Right(_) => arrow.indent(sym_.col-1+8),
            },
            match rh {
                Left(_) => sym("0").ws(o, " ").swim(o),
                Right(rh) => rh,
            },
        ));

        if let Left(rh) = rh {
            list_.push(Expr::Binary(
                Expr::Call(
                    sym("lfsr_data_read").indent(o, sym_.col-1).swim(o),
                    *lp,
                    vec![
                        (Some(sym("&lfs")), Some(tok(","))),
                        (Some(sym("&data").ws(o, " ")), Some(tok(","))),
                        args[4].clone(),
                        args[5].clone(),
                    ].swim(o),
                    *rp
                ).swim(o),
                tok("=>").ws(" "),
                rh.swim(o),
            ));
        }

        return Ok(squiggle(o, &list_));
    }

    Ok(expr)
}




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

    #[structopt(long)]
    dump_modified: bool,

    #[structopt(short="t", long)]
    in_toml: bool,
}

// entry point
fn main() -> Result<(), anyhow::Error> {
    let opt = Opt::from_args();

    if opt.in_toml {
        let f = File::open(opt.input.as_ref())?;
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
                        std::process::exit(0);
                    }

                    // parse
                    let mut tree = match parse(&tokens) {
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

                    // modify!
                    tree = tree.try_map_exprs(modify)?;

                    if opt.dump_modified {
                        println!("{:#?}", tree);
                        std::process::exit(0);
                    }

                    // flatten and write to file
                    tree.try_map_tokens(|_, tok| {
                        // make sure to keep whitespace!
                        write!(f_, "{}", tok.ws)?;
                        write!(f_, "{}", tok.tok)?;
                        Ok::<_, anyhow::Error>(tok)
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

        // modify!
        let tree = tree.try_map_exprs(modify)?;

        if opt.dump_modified {
            println!("{:#?}", tree);
            std::process::exit(0);
        }

        // flatten and write to file
        if let Some(output) = opt.output {
            let f = File::create(output)?;
            let mut f = BufWriter::new(f);
            tree.try_map_tokens(|_, tok| {
                // make sure to keep whitespace!
                write!(f, "{}", tok.ws)?;
                write!(f, "{}", tok.tok)?;
                Ok::<_, anyhow::Error>(tok)
            })?;
        }
    }

    Ok(())
}
