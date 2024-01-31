
use either::{Either, Left, Right};

use std::borrow::Cow;
use std::mem::transmute;
use std::cell::RefCell;

use crate::tokenizer::Token;
use crate::tokenizer::Tt;
use crate::errors::ParseError;
use crate::pool::Pooled;
use crate::pool::Pool;
use crate::pool::Swim;
use crate::pool::Pfork;


// tree stuff
#[derive(Debug, Clone, Copy)]
pub enum Expr<'b, 'a> {
    Sym(Token<'a>),
    Lit(Token<'a>),
    Decl(&'b Expr<'b, 'a>, Token<'a>),
    Call(&'b Expr<'b, 'a>, Token<'a>, &'b List<'b, 'a>, Token<'a>),
    Index(&'b Expr<'b, 'a>, Token<'a>, &'b List<'b, 'a>, Token<'a>),
    Block(&'b Expr<'b, 'a>, Token<'a>, &'b List<'b, 'a>, Token<'a>),
    Unary(Token<'a>, &'b Expr<'b, 'a>),
    Suffnary(&'b Expr<'b, 'a>, Token<'a>),
    Binary(&'b Expr<'b, 'a>, Token<'a>, &'b Expr<'b, 'a>),
    Ternary(&'b Expr<'b, 'a>, Token<'a>, &'b Expr<'b, 'a>, Token<'a>, &'b Expr<'b, 'a>),
    Squiggle(Token<'a>, &'b List<'b, 'a>, Token<'a>),
}

type List<'b, 'a> = [(Option<Expr<'b, 'a>>, Option<Token<'a>>)];

type Root<'b, 'a> = (&'b List<'b, 'a>, Token<'a>);

#[derive(Debug, Clone)]
pub struct Tree<'a>(Pooled<Root<'a, 'a>>);


// helper for pattern matching
struct Parser<'c, 'b, 'a> {
    // input state
    tokens: &'c [Token<'a>],
    i: usize,

    o: &'c mut Pool<'b>
}

impl<'c, 'b, 'a> Parser<'c, 'b, 'a> {
    fn new(tokens: &'c [Token<'a>], o: &'c mut Pool<'b>) -> Self {
        Self{
            tokens: tokens,
            i: 0,
            o: o,
        }
    }

    fn is_done(&self) -> bool {
        self.i >= self.tokens.len()
    }

    fn tail(&self) -> &'c [Token<'a>] {
        &self.tokens[self.i..]
    }

    fn tt(&self) -> Option<Tt> {
        self.tokens.get(self.i).map(|tok| tok.tt)
    }

    fn _next(&mut self, n: usize) {
        self.i += n;
    }

    fn munch(&mut self) -> Token<'a> {
        let tok = self.tokens[self.i];
        self._next(1);
        tok
    }

    fn error(&self, message: String) -> ParseError {
        let tok = &self.tokens[self.i];
        ParseError::new(tok.file.to_path_buf(), tok.line, tok.col, message)
    }

    fn unexpected(&self) -> ParseError {
        self.error(format!("Unexpected token: {:?}",
            self.tokens[self.i]
        ))
    }
}


// entry point
pub fn parse<'a>(
    file: &'a Path,
    tokens: &[Token<'a>]
) -> Result<Tree<'a>, ParseError> {
    // define parse rules
    fn parse_expr<'c, 'b, 'a>(
        p: &mut Parser<'c, 'b, 'a>
    ) -> Result<Option<Expr<'b, 'a>>, ParseError> {
        let mut lh = match p.tt() {
            Some(Tt::Sym) => Expr::Sym(p.munch()),
            Some(Tt::Number) => Expr::Lit(p.munch()),
            Some(Tt::String) => Expr::Lit(p.munch()),
            Some(Tt::Tilde) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.swim(&mut p.o),
            ),
            Some(Tt::Add) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.swim(&mut p.o),
            ),
            Some(Tt::Sub) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.swim(&mut p.o),
            ),
            Some(Tt::And) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.swim(&mut p.o),
            ),
            Some(Tt::Dot) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.swim(&mut p.o),
            ),
            Some(Tt::Not) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.swim(&mut p.o),
            ),
            Some(Tt::LParen) => Expr::Squiggle(
                p.munch(),
                parse_list(p)?.swim(&mut p.o),
                match p.tt() {
                    Some(Tt::RParen) => p.munch(),
                    _ => return Err(p.unexpected()),
                },
            ),
            // squiggles terminate
            Some(Tt::LSquiggle) => return Ok(Some(Expr::Squiggle(
                p.munch(),
                parse_list(p)?.swim(&mut p.o),
                match p.tt() {
                    Some(Tt::RSquiggle) => p.munch(),
                    _ => return Err(p.unexpected()),
                },
            ))),
            _ => return Ok(None),
        };

        loop {
            lh = match p.tt() {
                Some(Tt::Sym) => Expr::Decl(lh.swim(&mut p.o), p.munch()),
                Some(Tt::PlusPlus) => Expr::Suffnary(
                    lh.swim(&mut p.o),
                    p.munch(),
                ),
                Some(Tt::Dot) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Splat) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Slash) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Mod) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Add) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Sub) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::And) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::OrOr) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Eq) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Ne) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Le) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Ge) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Lt) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Gt) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Assign) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::AddAssign) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::SubAssign) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Arrow) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::BigArrow) => Expr::Binary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::Question) => Expr::Ternary(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_expr_required(p)?.swim(&mut p.o),
                    match p.tt() {
                        Some(Tt::Colon) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                    parse_expr_required(p)?.swim(&mut p.o),
                ),
                Some(Tt::LParen) => Expr::Call(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_list(p)?.swim(&mut p.o),
                    match p.tt() {
                        Some(Tt::RParen) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ),
                Some(Tt::LSquare) => Expr::Index(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_list(p)?.swim(&mut p.o),
                    match p.tt() {
                        Some(Tt::RSquare) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ),
                // squiggles terminate
                Some(Tt::LSquiggle) => return Ok(Some(Expr::Block(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_list(p)?.swim(&mut p.o),
                    match p.tt() {
                        Some(Tt::RSquiggle) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ))),
                _ => break,
            }
        }

        Ok(Some(lh))
    }

    fn parse_expr_required<'c, 'b, 'a>(
        p: &mut Parser<'c, 'b, 'a>
    ) -> Result<Expr<'b, 'a>, ParseError> {
        Ok(match parse_expr(p)? {
            Some(expr) => expr,
            None => return Err(p.unexpected()),
        })
    }

    fn parse_list<'c, 'b, 'a>(
        p: &mut Parser<'c, 'b, 'a>
    ) -> Result<Box<List<'b, 'a>>, ParseError> {
        let mut list = vec![];
        Ok(loop {
            let expr = parse_expr(p)?;
            let comma = match p.tt() {
                Some(Tt::Semi)  => Some(p.munch()),
                Some(Tt::Comma) => Some(p.munch()),
                _               => None,
            };

            match (&expr, &comma) {
                (Some(Expr::Squiggle(..)), _)
                    | (Some(Expr::Block(..)), _)
                    | (_, Some(_))
                => {
                    list.push((expr, comma));
                    continue;
                },
                (Some(_), _) => {
                    list.push((expr, comma));
                }
                (None, None) => {}
            }
            break list.into_boxed_slice();
        })
    }

    // parse inside a memory pool
    Ok(Tree(Pooled::try_from_fn(|o| {
        // create parser
        let mut p = Parser::new(tokens, o);

        // start parsing
        let root = parse_list(&mut p)?.swim(&mut p.o).as_ref();

        // just kind of shove any trailing whitespace into our tree
        let tws = if let Some(Tt::TrailingWs) = p.tt() {
            p.munch()
        } else {
            Token{
                file: file,
                line: tokens.last().map(|tok| tok.line).unwrap_or(1),
                col: tokens.last().map(|tok| tok.col).unwrap_or(1),
                tt: Tt::TrailingWs,
                lws: "",
                tok: "",
            }
        };

        // we should have consumed all tokens here
        if !p.is_done() {
            return Err(p.unexpected());
        }

        Ok((root, tws))
    })?))
}


//// traversals ////

// it's complicated, but this combined traversal impl means we only need to
// actually navigate the tree structure once
pub enum Spoon<'b, 'a> {
    Expr(Expr<'b, 'a>),
    Token(Token<'a>)
}

trait Pspoon<'b, 'a>: Sized {
    fn _try_spoon<E>(
        &self,
        cb: &mut dyn FnMut(&Spoon<'b, 'a>) -> Result<(), E>
    ) -> Result<(), E>;

    fn _try_pspoon<E>(
        &self,
        o: &mut Pool<'b>,
        cb: &mut dyn FnMut(&mut Pool<'b>, Spoon<'b, 'a>) -> Result<Spoon<'b, 'a>, E>
    ) -> Result<Self, E>;
}

impl<'b, 'a> Pspoon<'b, 'a> for Token<'a> {
    fn _try_spoon<E>(
        &self,
        cb: &mut dyn FnMut(&Spoon<'b, 'a>) -> Result<(), E>
    ) -> Result<(), E> {
        Ok(cb(&Spoon::Token(*self))?)
    }

    fn _try_pspoon<E>(
        &self,
        o: &mut Pool<'b>,
        cb: &mut dyn FnMut(&mut Pool<'b>, Spoon<'b, 'a>) -> Result<Spoon<'b, 'a>, E>
    ) -> Result<Self, E> {
        Ok(match cb(o, Spoon::Token(*self))? {
            Spoon::Token(self_) => self_,
            _ => unreachable!(),
        })
    }
}

impl<'b, 'a> Pspoon<'b, 'a> for Expr<'b, 'a> {
    fn _try_spoon<E>(
        &self,
        cb: &mut dyn FnMut(&Spoon<'b, 'a>) -> Result<(), E>
    ) -> Result<(), E> {
        Ok(cb(&Spoon::Expr(*self))?)
    }

    fn _try_pspoon<E>(
        &self,
        o: &mut Pool<'b>,
        cb: &mut dyn FnMut(&mut Pool<'b>, Spoon<'b, 'a>) -> Result<Spoon<'b, 'a>, E>
    ) -> Result<Self, E> {
        Ok(match cb(o, Spoon::Expr(*self))? {
            Spoon::Expr(self_) => self_,
            _ => unreachable!(),
        })
    }
}

// fork impls
impl<'b, 'a> Pfork<'b, Spoon<'b, 'a>> for Expr<'_, 'a> {
    type Pforked = Expr<'b, 'a>;

    fn _try_pfork<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Spoon<'b, 'a>) -> Result<Spoon<'b, 'a>, E>
    ) -> Result<Self::Pforked, E> {
        // map bottom-up so we are always guaranteed to make progress
        let expr = match *self {
            Expr::Sym(tok) => Expr::Sym(tok._try_pspoon(o, cb)?),
            Expr::Lit(tok) => Expr::Lit(tok._try_pspoon(o, cb)?),
            Expr::Decl(expr, tok) => Expr::Decl(
                expr._try_pfork(o, cb)?.swim(o),
                tok._try_pspoon(o, cb)?,
            ),
            Expr::Call(expr, l, list, r) => Expr::Call(
                expr._try_pfork(o, cb)?.swim(o),
                l._try_pspoon(o, cb)?,
                list._try_pfork(o, cb)?.swim(o),
                r._try_pspoon(o, cb)?,
            ),
            Expr::Index(expr, l, list, r) => Expr::Index(
                expr._try_pfork(o, cb)?.swim(o),
                l._try_pspoon(o, cb)?,
                list._try_pfork(o, cb)?.swim(o),
                r._try_pspoon(o, cb)?,
            ),
            Expr::Block(expr, l, list, r) => Expr::Block(
                expr._try_pfork(o, cb)?.swim(o),
                l._try_pspoon(o, cb)?,
                list._try_pfork(o, cb)?.swim(o),
                r._try_pspoon(o, cb)?,
            ),
            Expr::Unary(tok, expr) => Expr::Unary(
                tok._try_pspoon(o, cb)?,
                expr._try_pfork(o, cb)?.swim(o),
            ),
            Expr::Suffnary(expr, tok) => Expr::Suffnary(
                expr._try_pfork(o, cb)?.swim(o),
                tok._try_pspoon(o, cb)?,
            ),
            Expr::Binary(lh, tok, rh) => Expr::Binary(
                lh._try_pfork(o, cb)?.swim(o),
                tok._try_pspoon(o, cb)?,
                rh._try_pfork(o, cb)?.swim(o),
            ),
            Expr::Ternary(lh, l, mh, r, rh) => Expr::Ternary(
                lh._try_pfork(o, cb)?.swim(o),
                l._try_pspoon(o, cb)?,
                mh._try_pfork(o, cb)?.swim(o),
                r._try_pspoon(o, cb)?,
                rh._try_pfork(o, cb)?.swim(o),
            ),
            Expr::Squiggle(l, list, r) => Expr::Squiggle(
                l._try_pspoon(o, cb)?,
                list._try_pfork(o, cb)?.swim(o),
                r._try_pspoon(o, cb)?,
            ),
        };

        expr._try_pspoon(o, cb)
    }
}

impl<'b, 'a> Pfork<'b, Spoon<'b, 'a>> for List<'_, 'a> {
    type Pforked = Box<List<'b, 'a>>;

    fn _try_pfork<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Spoon<'b, 'a>) -> Result<Spoon<'b, 'a>, E>
    ) -> Result<Self::Pforked, E> {
        let mut list_ = vec![];
        for (expr, comma) in self.iter() {
            list_.push((
                expr.map(|expr| expr._try_pfork(o, cb)).transpose()?,
                comma.map(|comma| comma._try_pspoon(o, cb)).transpose()?
            ));
        }
        Ok(list_.into_boxed_slice())
    }
}

impl<'b, 'a> Pfork<'b, Spoon<'b, 'a>> for Root<'_, 'a> {
    type Pforked = Root<'b, 'a>;

    fn _try_pfork<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Spoon<'b, 'a>) -> Result<Spoon<'b, 'a>, E>
    ) -> Result<Self::Pforked, E> {
        Ok((
            self.0._try_pfork(o, cb)?.swim(o),
            self.1._try_pspoon(o, cb)?
        ))
    }
}


// now we can make these a bit prettier

// token traversal
impl<'b, 'a: 'b, T> Pfork<'b, Token<'a>> for T
where
    T: Pfork<'b, Spoon<'b, 'a>>
{
    type Pforked = T::Pforked;

    fn _try_pfork<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self::Pforked, E> {
        self._try_pfork(o, &mut |o, spoon: Spoon<'b, 'a>| {
            Ok(match spoon {
                Spoon::Token(tok) => Spoon::Token(cb(o, tok)?),
                spoon => spoon,
            })
        })
    }
}

impl<'a> Tree<'a> {
    pub fn fork_tokens<'b, F>(&self, mut cb: F) -> Self
    where
        F: FnMut(&mut Pool<'b>, Token<'a>) -> Token<'a>,
        'a: 'b
    {
        self.try_fork_tokens(|o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    pub fn try_fork_tokens<'b, F, E>(&self, cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'b>, Token<'a>) -> Result<Token<'a>, E>,
        'a: 'b
    {
        let root = self.0.try_pfork(cb)?;
        // I don't know why lifetime coercion in pfork only works _sometimes_,
        // 'a is strictly >= 'b, so this should coerce? It works fine in
        // from_fn. Oh well, all Rust problems can be solved with transmute
        let root = unsafe { transmute::<Pooled<Root<'b, 'a>>, Pooled<Root<'a, 'a>>>(root) };
        Ok(Tree(root))
    }

    // convenience wrappers
    pub fn map_tokens<'b, F>(self, cb: F) -> Self
    where
        F: FnMut(&mut Pool<'b>, Token<'a>) -> Token<'a>,
        'a: 'b
    {
        self.fork_tokens(cb)
    }

    pub fn try_map_tokens<'b, F, E>(self, cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'b>, Token<'a>) -> Result<Token<'a>, E>,
        'a: 'b
    {
        self.try_fork_tokens(cb)
    }

    pub fn visit_tokens<F>(&self, cb: F)
    where
        F: FnMut(&Token<'a>)
    {
        self.0.pvisit(cb)
    }

    pub fn try_visit_tokens<F, E>(&self, cb: F) -> Result<(), E>
    where
        F: FnMut(&Token<'a>) -> Result<(), E>
    {
        self.0.try_pvisit(cb)
    }
}


// expr traversal
impl<'b, 'a: 'b, T> Pfork<'b, Expr<'b, 'a>> for T
where
    T: Pfork<'b, Spoon<'b, 'a>>
{
    type Pforked = T::Pforked;

    fn _try_pfork<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Result<Expr<'b, 'a>, E>
    ) -> Result<Self::Pforked, E> {
        self._try_pfork(o, &mut |o, spoon: Spoon<'b, 'a>| {
            Ok(match spoon {
                Spoon::Expr(tok) => Spoon::Expr(cb(o, tok)?),
                spoon => spoon,
            })
        })
    }
}

impl<'a> Tree<'a> {
    pub fn fork_exprs<'b, F>(&self, mut cb: F) -> Self
    where
        F: FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Expr<'b, 'a>,
        'a: 'b
    {
        self.try_fork_exprs(|o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    pub fn try_fork_exprs<'b, F, E>(&self, cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Result<Expr<'b, 'a>, E>,
        'a: 'b
    {
        let root = self.0.try_pfork(cb)?;
        // I don't know why lifetime coercion in pfork only works _sometimes_,
        // 'a is strictly >= 'b, so this should coerce? It works fine in
        // from_fn. Oh well, all Rust problems can be solved with transmute
        let root = unsafe { transmute::<Pooled<Root<'b, 'a>>, Pooled<Root<'a, 'a>>>(root) };
        Ok(Tree(root))
    }

    // convenience wrappers
    pub fn map_exprs<'b, F>(self, cb: F) -> Self
    where
        F: FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Expr<'b, 'a>,
        'a: 'b
    {
        self.fork_exprs(cb)
    }

    pub fn try_map_exprs<'b, F, E>(self, cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Result<Expr<'b, 'a>, E>,
        'a: 'b
    {
        self.try_fork_exprs(cb)
    }

    pub fn visit_exprs<'b, F>(&self, cb: F)
    where
        F: FnMut(&Expr<'b, 'a>),
        'a: 'b
    {
        self.0.pvisit(cb)
    }

    pub fn try_visit_exprs<'b,  F, E>(&self, cb: F) -> Result<(), E>
    where
        F: FnMut(&Expr<'b, 'a>) -> Result<(), E>,
        'a: 'b
    {
        self.0.try_pvisit(cb)
    }
}



//// utils ///

use std::path::Path;

// whitespace stuff
impl<'b, 'a> Expr<'b, 'a> {
    pub fn file(&self) -> &'a Path {
        self.try_pvisit(|tok: &Token<'a>| Err(tok.file)).unwrap_err()
    }

    pub fn line(&self) -> usize {
        self.try_pvisit(|tok: &Token<'a>| Err(tok.line)).unwrap_err()
    }

    pub fn col(&self) -> usize {
        self.try_pvisit(|tok: &Token<'a>| Err(tok.col)).unwrap_err()
    }

    pub fn lws(&self) -> &'a str {
        self.try_pvisit(|tok: &Token<'a>| Err(tok.lws)).unwrap_err()
    }

    pub fn file_(self, o: &mut Pool<'b>, file: &'a Path) -> Self {
        let mut first = true;
        self.pmap(o, |_, tok: Token<'a>| {
            let tok = if first { tok.file_(file) } else { tok };
            first = false;
            tok
        })
    }

    pub fn line_(self, o: &mut Pool<'b>, line: usize) -> Self {
        let mut first = true;
        self.pmap(o, |_, tok: Token<'a>| {
            let tok = if first { tok.line_(line) } else { tok };
            first = false;
            tok
        })
    }

    pub fn col_(self, o: &mut Pool<'b>, col: usize) -> Self {
        let mut first = true;
        self.pmap(o, |_, tok: Token<'a>| {
            let tok = if first { tok.col_(col) } else { tok };
            first = false;
            tok
        })
    }

    pub fn lws_(self, o: &mut Pool<'b>, lws: &'a str) -> Self {
        let mut first = true;
        self.pmap(o, |_, tok: Token<'a>| {
            let tok = if first { tok.lws_(lws) } else { tok };
            first = false;
            tok
        })
    }

    pub fn indent(self, o: &mut Pool<'b>, n: usize) -> Self {
        let mut first = true;
        self.pmap(o, |_, tok: Token<'a>| {
            let tok = if first { tok.indent(n) } else { tok };
            first = false;
            tok
        })
    }
}


// extra utils, just to make tree creation easy
use crate::tokenizer::tok;
use std::borrow::Borrow;

pub fn sym<'a, S: Into<Cow<'a, str>>>(s: S) -> Expr<'static, 'a> {
    Expr::Sym(tok(s))
}

// a span is sort of an invisible squiggle, usually for injecting multiple
// exprs into a single expr
pub fn span<'b, 'a, E: Borrow<Expr<'b, 'a>>>(
    o: &mut Pool<'b>,
    list: &[E]
) -> Expr<'b, 'a> {
    let mut list_ = vec![];
    for (i, expr) in list.iter().enumerate() {
        list_.push((
            Some(expr.borrow().clone()),
            if i < list.len()-1 {
                Some(tok(";"))
            } else {
                None
            }
        ))
    }

    Expr::Squiggle(
        tok(""),
        list_.swim(o),
        tok(""),
    )
}


