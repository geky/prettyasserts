use std::borrow::Cow;
use std::mem::transmute;

use crate::tokenizer::Token;
use crate::tokenizer::Tt;
use crate::errors::ParseError;
use crate::pool::Pooled;
use crate::pool::Pool;
use crate::pool::Swim;
use crate::pool::Pmap;


// tree stuff
#[derive(Debug, Clone)]
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

type Root<'b, 'a> = (&'b List<'b, 'a>, Option<Token<'a>>);

#[derive(Debug)]
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
pub fn parse<'a>(tokens: &[Token<'a>]) -> Result<Tree<'a>, ParseError> {
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
            Some(Tt::LSquiggle) => Expr::Squiggle(
                p.munch(),
                parse_list(p)?.swim(&mut p.o),
                match p.tt() {
                    Some(Tt::RSquiggle) => p.munch(),
                    _ => return Err(p.unexpected()),
                },
            ),
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
                Some(Tt::LSquiggle) => Expr::Block(
                    lh.swim(&mut p.o),
                    p.munch(),
                    parse_list(p)?.swim(&mut p.o),
                    match p.tt() {
                        Some(Tt::RSquiggle) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ),
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

            if expr.is_some() || comma.is_some() {
                list.push((expr, comma));
            }
            if comma.is_none() {
                break list.into_boxed_slice();
            }
        })
    }

    // parse inside a memory swim
    let root = Pooled::try_from_fn(|o| {
        // create parser
        let mut p = Parser::new(tokens, o);

        // start parsing
        let root = parse_list(&mut p)?.swim(&mut p.o).as_ref();

        // just kind of shove any trailing whitespace into our tree
        let trailing_ws = if let Some(Tt::TrailingWs) = p.tt() {
            Some(p.munch())
        } else {
            None
        };

        // we should have consumed all tokens here
        if !p.is_done() {
            return Err(p.unexpected());
        }

        Ok((root, trailing_ws))
    })?;

    Ok(Tree(root))
}


//// traversals ////

// token traversal
impl<'a> Tree<'a> {
    pub fn map_tokens<'b, F>(&self, mut cb: F) -> Self
    where
        F: FnMut(&mut Pool<'b>, Token<'a>) -> Token<'a>,
        'a: 'b
    {
        self.try_map_tokens(|o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    pub fn try_map_tokens<'b, F, E>(&self, cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'b>, Token<'a>) -> Result<Token<'a>, E>,
        'a: 'b
    {
        let root = self.0.try_pmap(cb)?;
        // I don't know why lifetime coercion in pmap only works _sometimes_,
        // 'a is strictly >= 'b, so this should coerce? It works fine in
        // from_fn. Oh well, all Rust problems can be solved with transmute
        let root = unsafe { transmute::<Pooled<Root<'b, 'a>>, Pooled<Root<'a, 'a>>>(root) };
        Ok(Tree(root))
    }
}

impl<'b, 'a: 'b> Pmap<'b, Token<'a>> for Root<'_, 'a> {
    type Pmapped = Root<'b, 'a>;

    fn _try_pmap<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self::Pmapped, E> {
        Ok((
            self.0._try_pmap(o, cb)?.swim(o),
            self.1.map(|tok| cb(o, tok)).transpose()?
        ))
    }
}

impl<'b, 'a: 'b> Pmap<'b, Token<'a>> for List<'_, 'a> {
    type Pmapped = Box<List<'b, 'a>>;

    fn _try_pmap<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self::Pmapped, E> {
        let mut list_ = vec![];
        for (expr, comma) in self.iter() {
            list_.push((
                expr.as_ref().map(|expr| expr._try_pmap(o, cb)).transpose()?,
                comma.map(|tok| cb(o, tok)).transpose()?
            ));
        }
        Ok(list_.into_boxed_slice())
    }
}

impl<'b, 'a: 'b> Pmap<'b, Token<'a>> for Expr<'_, 'a> {
    type Pmapped = Expr<'b, 'a>;

    fn _try_pmap<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self::Pmapped, E> {
        Ok(match self {
            Expr::Sym(tok) => Expr::Sym(cb(o, *tok)?),
            Expr::Lit(tok) => Expr::Lit(cb(o, *tok)?),
            Expr::Decl(expr, tok) => Expr::Decl(
                expr._try_pmap(o, cb)?.swim(o),
                cb(o, *tok)?,
            ),
            Expr::Call(expr, l, list, r) => Expr::Call(
                expr._try_pmap(o, cb)?.swim(o),
                cb(o, *l)?,
                list._try_pmap(o, cb)?.swim(o),
                cb(o, *r)?,
            ),
            Expr::Index(expr, l, list, r) => Expr::Index(
                expr._try_pmap(o, cb)?.swim(o),
                cb(o, *l)?,
                list._try_pmap(o, cb)?.swim(o),
                cb(o, *r)?,
            ),
            Expr::Block(expr, l, list, r) => Expr::Block(
                expr._try_pmap(o, cb)?.swim(o),
                cb(o, *l)?,
                list._try_pmap(o, cb)?.swim(o),
                cb(o, *r)?,
            ),
            Expr::Unary(tok, expr) => Expr::Unary(
                cb(o, *tok)?,
                expr._try_pmap(o, cb)?.swim(o),
            ),
            Expr::Suffnary(expr, tok) => Expr::Suffnary(
                expr._try_pmap(o, cb)?.swim(o),
                cb(o, *tok)?,
            ),
            Expr::Binary(lh, tok, rh) => Expr::Binary(
                lh._try_pmap(o, cb)?.swim(o),
                cb(o, *tok)?,
                rh._try_pmap(o, cb)?.swim(o),
            ),
            Expr::Ternary(lh, l, mh, r, rh) => Expr::Ternary(
                lh._try_pmap(o, cb)?.swim(o),
                cb(o, *l)?,
                mh._try_pmap(o, cb)?.swim(o),
                cb(o, *r)?,
                rh._try_pmap(o, cb)?.swim(o),
            ),
            Expr::Squiggle(l, list, r) => Expr::Squiggle(
                cb(o, *l)?,
                list._try_pmap(o, cb)?.swim(o),
                cb(o, *r)?,
            ),
        })
    }
}


// expr traversal
impl<'a> Tree<'a> {
    pub fn map_exprs<'b, F>(&self, mut cb: F) -> Self
    where
        F: FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Expr<'b, 'a>,
        'a: 'b
    {
        self.try_map_exprs(|o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    pub fn try_map_exprs<'b, F, E>(&self, cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Result<Expr<'b, 'a>, E>,
        'a: 'b
    {
        let root = self.0.try_pmap(cb)?;
        // I don't know why lifetime coercion in pmap only works _sometimes_,
        // 'a is strictly >= 'b, so this should coerce? It works fine in
        // from_fn. Oh well, all Rust problems can be solved with transmute
        let root = unsafe { transmute::<Pooled<Root<'b, 'a>>, Pooled<Root<'a, 'a>>>(root) };
        Ok(Tree(root))
    }
}

impl<'b, 'a: 'b> Pmap<'b, Expr<'b, 'a>> for Root<'_, 'a> {
    type Pmapped = Root<'b, 'a>;

    fn _try_pmap<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Result<Expr<'b, 'a>, E>
    ) -> Result<Self::Pmapped, E> {
        Ok((
            self.0._try_pmap(o, cb)?.swim(o),
            self.1
        ))
    }
}

impl<'b, 'a: 'b> Pmap<'b, Expr<'b, 'a>> for List<'_, 'a> {
    type Pmapped = Box<List<'b, 'a>>;

    fn _try_pmap<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Result<Expr<'b, 'a>, E>
    ) -> Result<Self::Pmapped, E> {
        let mut list_ = vec![];
        for (expr, comma) in self.iter() {
            list_.push((
                expr.as_ref().map(|expr| expr._try_pmap(o, cb)).transpose()?,
                *comma
            ));
        }
        Ok(list_.into_boxed_slice())
    }
}

impl<'b, 'a: 'b> Pmap<'b, Expr<'b, 'a>> for Expr<'_, 'a> {
    type Pmapped = Expr<'b, 'a>;

    fn _try_pmap<E>(
        &self,
        o: &mut Pool<'b>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'b>, Expr<'b, 'a>) -> Result<Expr<'b, 'a>, E>
    ) -> Result<Self::Pmapped, E> {
        // map bottom-up so we are always guaranteed to make progress
        let expr = match self {
            Expr::Sym(tok) => Expr::Sym(*tok),
            Expr::Lit(tok) => Expr::Lit(*tok),
            Expr::Decl(expr, tok) => Expr::Decl(
                expr._try_pmap(o, cb)?.swim(o),
                *tok,
            ),
            Expr::Call(expr, l, list, r) => Expr::Call(
                expr._try_pmap(o, cb)?.swim(o),
                *l,
                list._try_pmap(o, cb)?.swim(o),
                *r,
            ),
            Expr::Index(expr, l, list, r) => Expr::Index(
                expr._try_pmap(o, cb)?.swim(o),
                *l,
                list._try_pmap(o, cb)?.swim(o),
                *r,
            ),
            Expr::Block(expr, l, list, r) => Expr::Block(
                expr._try_pmap(o, cb)?.swim(o),
                *l,
                list._try_pmap(o, cb)?.swim(o),
                *r,
            ),
            Expr::Unary(tok, expr) => Expr::Unary(
                *tok,
                expr._try_pmap(o, cb)?.swim(o),
            ),
            Expr::Suffnary(expr, tok) => Expr::Suffnary(
                expr._try_pmap(o, cb)?.swim(o),
                *tok,
            ),
            Expr::Binary(lh, tok, rh) => Expr::Binary(
                lh._try_pmap(o, cb)?.swim(o),
                *tok,
                rh._try_pmap(o, cb)?.swim(o),
            ),
            Expr::Ternary(lh, l, mh, r, rh) => Expr::Ternary(
                lh._try_pmap(o, cb)?.swim(o),
                *l,
                mh._try_pmap(o, cb)?.swim(o),
                *r,
                rh._try_pmap(o, cb)?.swim(o),
            ),
            Expr::Squiggle(l, list, r) => Expr::Squiggle(
                *l,
                list._try_pmap(o, cb)?.swim(o),
                *r,
            ),
        };

        cb(o, expr)
    }
}


//// utils ///

// whitespace stuff
impl<'a> Tree<'a> {
    pub fn ws(self, ws: &'a str) -> Self {
        let mut first = true;
        let ws = ws.into();
        self.map_tokens(|_, tok| {
            if first {
                let tok = tok.ws(ws);
                first = false;
                tok
            } else {
                tok
            }
        })
    }

    pub fn indent(self, n: usize) -> Self {
        let mut first = true;
        self.map_tokens(|_, tok: Token<'a>| {
            if first {
                let tok = tok.indent(n);
                first = false;
                tok
            } else {
                tok
            }
        })
    }
}

impl<'b, 'a> Expr<'b, 'a> {
    pub fn ws(self, o: &mut Pool<'b>, ws: &'a str) -> Self {
        let mut first = true;
        self.pmap(o, |_, tok: Token<'a>| {
            if first {
                let tok = tok.ws(ws);
                first = false;
                tok
            } else {
                tok
            }
        })
    }

    pub fn indent(self, o: &mut Pool<'b>, n: usize) -> Self {
        let mut first = true;
        self.pmap(o, |_, tok: Token<'a>| {
            if first {
                let tok = tok.indent(n);
                first = false;
                tok
            } else {
                tok
            }
        })
    }
}

// other traits
impl<'a> Clone for Tree<'a> {
    fn clone(&self) -> Self {
        self.map_tokens(|_, t| t)
    }
}

