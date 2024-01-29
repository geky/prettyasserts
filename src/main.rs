#![allow(dead_code)]

use structopt::StructOpt;
use anyhow;
use either::{Left, Right};

use std::path::PathBuf;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;
use std::ops::Deref;
use std::borrow::Cow;

// The actual parser is over here
mod errors;
mod tokenizer;
use tokenizer::tokenize;
use tokenizer::Token;
use tokenizer::Tt;
mod parser;
use parser::parse;
use parser::Expr;


fn tok<'a>(s: &'a str) -> Token<'a> {
    Token::new(Tt::Sym, s)
}

fn sym<'a>(s: &'a str) -> Expr<'a> {
    Expr::Sym(tok(s))
}

fn squiggle<'a>(exprs: &[Expr<'a>]) -> Expr<'a> {
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
        list,
        tok(""),
    )
}


// modify the tree
fn modify<'a>(expr: Expr<'a>) -> Result<Expr<'a>, anyhow::Error> {
    if let Expr::Binary(lh, Token{tt: Tt::BigArrow,..}, rh) = &expr {
        if let Expr::Call(sym_, l, list, r) = lh.deref() {
            if let Expr::Sym(sym_@Token{tok: Cow::Borrowed("lfsr_rbyd_get"), ..}) = sym_.deref() {
                let rh = match rh.deref() {
                    rh@Expr::Sym(sym_) if sym_.tok.starts_with("LFS_ERR") => Right(rh),
                    rh => Left(rh),
                };

                let mut list_ = vec![];
                list_.push(Expr::Binary(
                    Box::new(Expr::Call(
                        Box::new(sym("lfsr_rbyd_lookup").ws(sym_.ws.clone())),
                        l.clone(),
                        vec![
                            list[0].clone(),
                            list[1].clone(),
                            list[2].clone(),
                            list[3].clone(),
                            (Some(sym("&data").indent(sym_.col-1 + 8)), None)
                        ],
                        r.clone(),
                    )),
                    tok("=>").ws(" "),
                    Box::new(match rh {
                        Left(_) => sym("0").ws(" "),
                        Right(rh) => rh.clone(),
                    }),
                ));

                if let Left(rh) = rh {
                    list_.push(Expr::Binary(
                        Box::new(Expr::Call(
                            Box::new(sym("lfsr_data_read").indent(sym_.col-1)),
                            l.clone(),
                            vec![
                                (Some(sym("&lfs")), Some(tok(","))),
                                (Some(sym("&data").ws(" ")), Some(tok(","))),
                                list[4].clone(),
                                list[5].clone(),
                            ],
                            r.clone()
                        )),
                        tok("=>").ws(" "),
                        Box::new(rh.clone()),
                    ));
                }

                return Ok(squiggle(&list_));
            }
        }
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

    // modify!
    let tree = tree.try_map_exprs(modify)?;

    if opt.dump_modified {
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
