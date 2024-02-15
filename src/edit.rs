
use either::{Left, Right};

use std::ops::Deref;

use crate::rc::Rc;
use crate::rc::Transborrow;
use crate::tokenizer::Token;
use crate::tokenizer::Tt;
use crate::tokenizer::tok;
use crate::parser::{Expr, Expr_};
use crate::parser::sym;
use crate::parser::span;


fn cmp_name(tt: Tt) -> &'static str {
    match tt {
        Tt::Eq => "eq",
        Tt::Ne => "ne",
        Tt::Lt => "lt",
        Tt::Gt => "gt",
        Tt::Le => "le",
        Tt::Ge => "ge",
        _ => unreachable!(),
    }
}


// edit the tree
pub fn edit<'a>(expr: Expr<'a>) -> Result<Expr<'a>, anyhow::Error> {
    Ok(match expr.borrow() {
        // assert(memcmp(a, b, n) ?= 0)
        Expr_::Call(
            Expr_::Sym(assert@Token{tok: "assert", ..}),
            lp,
            &[
                (
                    Some(Expr_::Binary(
                        Expr_::Call(
                            Expr_::Sym(Token{tok: "memcmp", ..}),
                            _,
                            &[
                                (Some(lh), Some(comma1)),
                                (Some(rh), Some(comma2)),
                                (Some(n), None),
                            ],
                            _,
                        ),
                        cmp@Token{
                            tt: Tt::Eq
                                | Tt::Ne
                                | Tt::Lt
                                | Tt::Gt
                                | Tt::Le
                                | Tt::Ge,
                            ..
                        },
                        Expr_::Lit(Token{tok: "0", ..}),
                    )),
                    None
                )
            ],
            rp,
        ) => {
            Expr::Call(
                Rc::new(sym(
                    format!("__PRETTY_ASSERT_MEM_{}",
                        cmp_name(cmp.tt).to_ascii_uppercase()
                    )
                ).lws_(expr.lws())),
                *lp,
                Rc::from(vec![
                    (
                        Some(Expr::from(lh)),
                        Some(comma1)
                    ),
                    (
                        Some(Expr::from(rh)),
                        Some(comma2)
                    ),
                    (
                        Some(Expr::from(n)),
                        None
                    )
                ]),
                *rp,
            )
        }

        // assert(strcmp(a, b) ?= 0)
        Expr_::Call(
            Expr_::Sym(assert@Token{tok: "assert", ..}),
            lp,
            &[
                (
                    Some(Expr_::Binary(
                        Expr_::Call(
                            Expr_::Sym(Token{tok: "strcmp", ..}),
                            _,
                            &[
                                (Some(lh), Some(comma)),
                                (Some(rh), None),
                            ],
                            _,
                        ),
                        cmp@Token{
                            tt: Tt::Eq
                                | Tt::Ne
                                | Tt::Lt
                                | Tt::Gt
                                | Tt::Le
                                | Tt::Ge,
                            ..
                        },
                        Expr_::Lit(Token{tok: "0", ..}),
                    )),
                    None
                )
            ],
            rp,
        ) => {
            Expr::Call(
                Rc::new(sym(
                    format!("__PRETTY_ASSERT_STR_{}",
                        cmp_name(cmp.tt).to_ascii_uppercase()
                    )
                ).lws_(expr.lws())),
                *lp,
                Rc::from(vec![
                    (
                        Some(Expr::from(lh)),
                        Some(comma)
                    ),
                    (
                        Some(Expr::from(rh)),
                        None
                    )
                ]),
                *rp,
            )
        }

        // assert(a ?= b)
        Expr_::Call(
            Expr_::Sym(assert@Token{tok: "assert", ..}),
            lp,
            &[
                (
                    Some(Expr_::Binary(
                        lh,
                        cmp@Token{
                            tt: Tt::Eq
                                | Tt::Ne
                                | Tt::Lt
                                | Tt::Gt
                                | Tt::Le
                                | Tt::Ge,
                            ..
                        },
                        rh,
                    )),
                    None
                )
            ],
            rp,
        ) => {
            Expr::Call(
                Rc::new(sym(
                    format!("__PRETTY_ASSERT_INT_{}",
                        cmp_name(cmp.tt).to_ascii_uppercase()
                    )
                ).lws_(expr.lws())),
                *lp,
                Rc::from(vec![
                    (
                        Some(Expr::from(lh)),
                        Some(tok(",").lws_(cmp.lws))
                    ),
                    (
                        Some(Expr::from(rh)),
                        None
                    )
                ]),
                *rp,
            )
        }

        // assert(a)
        Expr_::Call(
            Expr_::Sym(assert@Token{tok: "assert", ..}),
            lp,
            &[
                (Some(expr_), None),
            ],
            rp,
        ) => {
            Expr::Call(
                Rc::new(sym("__PRETTY_ASSERT_BOOL_EQ").lws_(expr.lws())),
                *lp,
                Rc::from(vec![
                    (
                        Some(Expr::from(expr_)),
                        Some(tok(","))
                    ),
                    (
                        Some(sym("true").lws_(" ")),
                        None
                    )
                ]),
                *rp,
            )
        }

        // unreachable()
        Expr_::Call(
            Expr_::Sym(unreachable@Token{tok: "unreachable", ..}),
            lp,
            &[],
            rp,
        ) => {
            Expr::Call(
                Rc::new(sym("__PRETTY_ASSERT_UNREACHABLE").lws_(expr.lws())),
                *lp,
                Rc::from(vec![]),
                *rp,
            )
        }

        // memcmp(a, b, n) => 0
        Expr_::Binary(
            Expr_::Call(
                Expr_::Sym(Token{tok: "memcmp", ..}),
                _,
                &[
                    (Some(lh), Some(comma1)),
                    (Some(rh), Some(comma2)),
                    (Some(n), None),
                ],
                _,
            ),
            arrow@Token{tt: Tt::BigArrow, ..},
            Expr_::Lit(Token{tok: "0", ..}),
        ) => {
            Expr::Call(
                Rc::new(sym("__PRETTY_ASSERT_MEM_EQ").lws_(expr.lws())),
                tok("("),
                Rc::from(vec![
                    (
                        Some(Expr::from(lh)),
                        Some(comma1)
                    ),
                    (
                        Some(Expr::from(rh)),
                        Some(comma2)
                    ),
                    (
                        Some(Expr::from(n)),
                        None
                    )
                ]),
                tok(")"),
            )
        }

        // strcmp(a, b) => 0
        Expr_::Binary(
            Expr_::Call(
                Expr_::Sym(Token{tok: "strcmp", ..}),
                _,
                &[
                    (Some(lh), Some(comma)),
                    (Some(rh), None),
                ],
                _,
            ),
            arrow@Token{tt: Tt::BigArrow, ..},
            Expr_::Lit(Token{tok: "0", ..}),
        ) => {
            Expr::Call(
                Rc::new(sym("__PRETTY_ASSERT_STR_EQ").lws_(expr.lws())),
                tok("("),
                Rc::from(vec![
                    (
                        Some(Expr::from(lh)),
                        Some(comma)
                    ),
                    (
                        Some(Expr::from(rh)),
                        None
                    )
                ]),
                tok(")"),
            )
        }

        // a => b
        Expr_::Binary(
            &lh,
            arrow@Token{tt: Tt::BigArrow, ..},
            &rh
        ) => {
            Expr::Call(
                Rc::new(sym("__PRETTY_ASSERT_INT_EQ").lws_(expr.lws())),
                tok("("),
                Rc::from(vec![
                    (
                        Some(Expr::from(lh).lws_("")),
                        Some(tok(",").lws_(arrow.lws))
                    ),
                    (
                        Some(Expr::from(rh)),
                        None
                    )
                ]),
                tok(")"),
            )
        }
        _ => expr,
    })
}

