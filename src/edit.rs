
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


// edit the tree
pub fn edit<'a>(expr: Expr<'a>) -> Result<Expr<'a>, anyhow::Error> {
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

