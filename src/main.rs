#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use structopt::StructOpt;
use anyhow;
use either::{Left, Right};

use std::path::PathBuf;
use std::fs;
use std::fs::File;
use std::ops::Deref;
use std::borrow::Cow;
use std::io::BufReader;
use std::io::BufRead;
use std::io::BufWriter;
use std::io::Write;


// The actual parser is over here
mod errors;
mod rc;
use rc::Rc;
use rc::Transmute;
use rc::Transborrow;
mod tokenizer;
use tokenizer::tokenize;
use tokenizer::tokenize_at;
use tokenizer::Token;
use tokenizer::Tt;
use tokenizer::tok;
mod parser;
use parser::parse;
use parser::{Expr, Expr_};
use parser::sym;
use parser::span;
use parser::Map;


// edit the tree
fn edit<'a>(expr: Expr<'a>) -> Result<Expr<'a>, anyhow::Error> {
    if let
        index@Expr_::Index(
            Expr_::Decl(
                Expr_::Sym(Token{tok: "uint8_t", ..}),
                Token{tok: "buffer", ..},
            ),
            ..
        ) = expr.borrow()
    {
        let data = sym("lfsr_data_t data").indent(index.col()-1);
        return Ok(span(&[
            data,
            index.into(),
        ]));
    }

    if let
        Expr_::Binary(
            Expr_::Call(
                Expr_::Sym(sym_@Token{tok: "lfsr_rbyd_get", ..}),
                lp,
                &[
                    (Some(lfs),     c0),
                    (Some(rbyd),    c1),
                    (Some(rid),     c2),
                    (Some(tag),     c3),
                    (Some(buffer),  c4),
                    (Some(size),    c5),
                ],
                rp,
            ),
            arrow@Token{tt: Tt::BigArrow, ..},
            rh
        ) = expr.borrow()
    {
        // left => no error
        // right => error
        let rh = match rh {
            rh@Expr_::Sym(sym_) if sym_.tok.starts_with("LFS_ERR_") => Right(rh),
            rh => Left(rh),
        };

        let mut list_ = vec![];
        list_.push(Expr::Binary(
            Rc::new(Expr::Call(
                Rc::new(sym("lfsr_rbyd_lookup").lws_(sym_.lws)),
                *lp,
                Rc::from(vec![
                    (Some(lfs.into()),  c0),
                    (Some(rbyd.into()), c1),
                    (Some(rid.into()),  c2),
                    (Some(tag.into()),  c3),
                    (Some(match rh {
                        Left(_) => sym("&data").lws_(" "),
                        Right(_) => sym("&data").indent(sym_.col-1+8),
                    }), None)
                ]),
                *rp,
            )),
            arrow.lws_(" "),
            Rc::new(match rh {
                Left(_) => sym("0").lws_(" "),
                Right(rh) => rh.deref().into(),
            }),
        ));

        if let Left(rh) = rh {
            list_.push(Expr::Binary(
                Rc::new(Expr::Call(
                    Rc::new(sym("lfsr_data_read").indent(sym_.col-1)),
                    *lp,
                    Rc::from(vec![
                        (Some(sym("&lfs")), Some(tok(","))),
                        (Some(sym("&data").lws_(" ")), Some(tok(","))),
                        (Some(buffer.into()),   c4),
                        (Some(size.into()),     c5),
                    ]),
                    *rp
                )),
                tok("=>").lws_(" "),
                Rc::new(rh.deref().into()),
            ));
        }

        return Ok(span(&list_));
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
