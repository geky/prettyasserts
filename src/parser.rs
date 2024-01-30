use std::borrow::Cow;
use std::mem::transmute;

use crate::tokenizer::Token;
use crate::tokenizer::Tt;
use crate::errors::ParseError;
use crate::pool::Pooled;
use crate::pool::Pool;
use crate::pool::Swim;
use crate::pool::Map;


// tree stuff
#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Sym(Token<'a>),
    Lit(Token<'a>),
    Decl(&'a Expr<'a>, Token<'a>),
    Call(&'a Expr<'a>, Token<'a>, &'a List<'a>, Token<'a>),
    Index(&'a Expr<'a>, Token<'a>, &'a List<'a>, Token<'a>),
    Block(&'a Expr<'a>, Token<'a>, &'a List<'a>, Token<'a>),
    Unary(Token<'a>, &'a Expr<'a>),
    Suffnary(&'a Expr<'a>, Token<'a>),
    Binary(&'a Expr<'a>, Token<'a>, &'a Expr<'a>),
    Ternary(&'a Expr<'a>, Token<'a>, &'a Expr<'a>, Token<'a>, &'a Expr<'a>),
    Squiggle(Token<'a>, &'a List<'a>, Token<'a>),
}

type List<'a> = [(Option<Expr<'a>>, Option<Token<'a>>)];

type Root<'a> = (Box<List<'a>>, Option<Token<'a>>);

#[derive(Debug)]
pub struct Tree<'a>(Pooled<'a, Root<'a>>);


// helper for pattern matching
struct Parser<'b, 'a> {
    // input state
    tokens: &'b [Token<'a>],
    i: usize,

    o: &'b mut Pool<'a>
}

impl<'b, 'a> Parser<'b, 'a> {
    fn new(tokens: &'b [Token<'a>], o: &'b mut Pool<'a>) -> Self {
        Self{
            tokens: tokens,
            i: 0,
            o: o,
        }
    }

    fn is_done(&self) -> bool {
        self.i >= self.tokens.len()
    }

    fn tail(&self) -> &'b [Token<'a>] {
        &self.tokens[self.i..]
    }

    fn tt(&self) -> Option<Tt> {
        self.tokens.get(self.i).map(|tok| tok.tt)
    }

    fn _next(&mut self, n: usize) {
        self.i += n;
    }

    fn munch(&mut self) -> Token<'a> {
        let tok = self.tokens[self.i].clone();
        self._next(1);
        tok
    }

    fn error(&self, message: String) -> ParseError {
        let tok = &self.tokens[self.i];
        ParseError::new(&tok.file, tok.line, tok.col, message)
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
    fn parse_expr<'a>(
        p: &mut Parser<'_, 'a>
    ) -> Result<Option<Expr<'a>>, ParseError> {
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

    fn parse_expr_required<'a>(
        p: &mut Parser<'_, 'a>
    ) -> Result<Expr<'a>, ParseError> {
        Ok(match parse_expr(p)? {
            Some(expr) => expr,
            None => return Err(p.unexpected()),
        })
    }

    fn parse_list<'a>(
        p: &mut Parser<'_, 'a>
    ) -> Result<Box<List<'a>>, ParseError> {
        let mut list = vec![];
        Ok(loop {
            let expr = parse_expr(p)?;
            let comma = match p.tt() {
                Some(Tt::Semi)  => Some(p.munch()),
                Some(Tt::Comma) => Some(p.munch()),
                _               => None,
            };

            if expr.is_some() || comma.is_some() {
                list.push((expr, comma.clone()));
            }
            if comma.is_none() {
                break list.into_boxed_slice();
            }
        })
    }

    // parse inside a memory swim
    let root = Pooled::try_from_fn(|o| {
        // create parser
        #[allow(mutable_transmutes)]
        let mut p = Parser::new(tokens, unsafe {transmute(o)});

        // start parsing
        let root = parse_list(&mut p)?;

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


// token traversal
impl<'a> Tree<'a> {
    pub fn map_tokens<F>(&self, mut cb: F) -> Self
    where
        F: FnMut(&mut Pool<'a>, Token<'a>) -> Token<'a>
    {
        self.try_map_tokens(|o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    pub fn try_map_tokens<F, E>(&self, cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'a>, Token<'a>) -> Result<Token<'a>, E>
    {
        Ok(Tree(self.0.try_map(cb)?))
    }
}

impl<'a> Map<'a, Token<'a>> for Root<'a> {
    type Mapped = Self;

    fn _try_map<E>(
        &self,
        p: &mut Pool<'a>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'a>, Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self::Mapped, E> {
        Ok((
            self.0.as_ref()._try_map(p, cb)?,
            self.1.as_ref().map(|tok| cb(p, tok.clone())).transpose()?
        ))
    }
}

impl<'a> Map<'a, Token<'a>> for List<'a> {
    type Mapped = Box<List<'a>>;

    fn _try_map<E>(
        &self,
        p: &mut Pool<'a>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'a>, Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self::Mapped, E> {
        let mut list_ = vec![];
        for (expr, comma) in self.iter() {
            list_.push((
                expr.as_ref().map(|expr| expr._try_map(p, cb)).transpose()?,
                comma.as_ref().map(|tok| cb(p, tok.clone())).transpose()?
            ));
        }
        Ok(list_.into_boxed_slice())
    }
}

impl<'a> Map<'a, Token<'a>> for Expr<'a> {
    type Mapped = Self;

    fn _try_map<E>(
        &self,
        p: &mut Pool<'a>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'a>, Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self::Mapped, E> {
        Ok(match self {
            Expr::Sym(tok) => Expr::Sym(cb(p, tok.clone())?),
            Expr::Lit(tok) => Expr::Lit(cb(p, tok.clone())?),
            Expr::Decl(expr, tok) => Expr::Decl(
                expr._try_map(p, cb)?.swim(p),
                cb(p, tok.clone())?,
            ),
            Expr::Call(expr, l, list, r) => Expr::Call(
                expr._try_map(p, cb)?.swim(p),
                cb(p, l.clone())?,
                list._try_map(p, cb)?.swim(p),
                cb(p, r.clone())?,
            ),
            Expr::Index(expr, l, list, r) => Expr::Index(
                expr._try_map(p, cb)?.swim(p),
                cb(p, l.clone())?,
                list._try_map(p, cb)?.swim(p),
                cb(p, r.clone())?,
            ),
            Expr::Block(expr, l, list, r) => Expr::Block(
                expr._try_map(p, cb)?.swim(p),
                cb(p, l.clone())?,
                list._try_map(p, cb)?.swim(p),
                cb(p, r.clone())?,
            ),
            Expr::Unary(tok, expr) => Expr::Unary(
                cb(p, tok.clone())?,
                expr._try_map(p, cb)?.swim(p),
            ),
            Expr::Suffnary(expr, tok) => Expr::Suffnary(
                expr._try_map(p, cb)?.swim(p),
                cb(p, tok.clone())?,
            ),
            Expr::Binary(lh, tok, rh) => Expr::Binary(
                lh._try_map(p, cb)?.swim(p),
                cb(p, tok.clone())?,
                rh._try_map(p, cb)?.swim(p),
            ),
            Expr::Ternary(lh, l, mh, r, rh) => Expr::Ternary(
                lh._try_map(p, cb)?.swim(p),
                cb(p, l.clone())?,
                mh._try_map(p, cb)?.swim(p),
                cb(p, r.clone())?,
                rh._try_map(p, cb)?.swim(p),
            ),
            Expr::Squiggle(l, list, r) => Expr::Squiggle(
                cb(p, l.clone())?,
                list._try_map(p, cb)?.swim(p),
                cb(p, r.clone())?,
            ),
        })
    }
}


//// expr traversal
impl<'a> Tree<'a> {
    pub fn map_exprs<F>(&self, mut cb: F) -> Self
    where
        F: FnMut(&mut Pool<'a>, Expr<'a>) -> Expr<'a>
    {
        self.try_map_exprs(|o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    pub fn try_map_exprs<F, E>(&self, cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'a>, Expr<'a>) -> Result<Expr<'a>, E>
    {
        Ok(Tree(self.0.try_map(cb)?))
    }
}

impl<'a> Map<'a, Expr<'a>> for Root<'a> {
    type Mapped = Self;

    fn _try_map<E>(
        &self,
        p: &mut Pool<'a>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'a>, Expr<'a>) -> Result<Expr<'a>, E>
    ) -> Result<Self::Mapped, E> {
        Ok((
            self.0.as_ref()._try_map(p, cb)?,
            self.1.clone()
        ))
    }
}

impl<'a> Map<'a, Expr<'a>> for List<'a> {
    type Mapped = Box<List<'a>>;

    fn _try_map<E>(
        &self,
        p: &mut Pool<'a>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'a>, Expr<'a>) -> Result<Expr<'a>, E>
    ) -> Result<Self::Mapped, E> {
        let mut list_ = vec![];
        for (expr, comma) in self.iter() {
            list_.push((
                expr.as_ref().map(|expr| expr._try_map(p, cb)).transpose()?,
                comma.clone()
            ));
        }
        Ok(list_.into_boxed_slice())
    }
}

impl<'a> Map<'a, Expr<'a>> for Expr<'a> {
    type Mapped = Self;

    fn _try_map<E>(
        &self,
        p: &mut Pool<'a>,
        // this recursion makes the compiler explode if generic!
        cb: &mut dyn FnMut(&mut Pool<'a>, Expr<'a>) -> Result<Expr<'a>, E>
    ) -> Result<Self, E> {
        // map bottom-up so we are always guaranteed to make progress
        let expr = match self {
            Expr::Sym(tok) => Expr::Sym(tok.clone()),
            Expr::Lit(tok) => Expr::Lit(tok.clone()),
            Expr::Decl(expr, tok) => Expr::Decl(
                expr._try_map(p, cb)?.swim(p),
                tok.clone(),
            ),
            Expr::Call(expr, l, list, r) => Expr::Call(
                expr._try_map(p, cb)?.swim(p),
                l.clone(),
                list._try_map(p, cb)?.swim(p),
                r.clone(),
            ),
            Expr::Index(expr, l, list, r) => Expr::Index(
                expr._try_map(p, cb)?.swim(p),
                l.clone(),
                list._try_map(p, cb)?.swim(p),
                r.clone(),
            ),
            Expr::Block(expr, l, list, r) => Expr::Block(
                expr._try_map(p, cb)?.swim(p),
                l.clone(),
                list._try_map(p, cb)?.swim(p),
                r.clone(),
            ),
            Expr::Unary(tok, expr) => Expr::Unary(
                tok.clone(),
                expr._try_map(p, cb)?.swim(p),
            ),
            Expr::Suffnary(expr, tok) => Expr::Suffnary(
                expr._try_map(p, cb)?.swim(p),
                tok.clone(),
            ),
            Expr::Binary(lh, tok, rh) => Expr::Binary(
                lh._try_map(p, cb)?.swim(p),
                tok.clone(),
                rh._try_map(p, cb)?.swim(p),
            ),
            Expr::Ternary(lh, l, mh, r, rh) => Expr::Ternary(
                lh._try_map(p, cb)?.swim(p),
                l.clone(),
                mh._try_map(p, cb)?.swim(p),
                r.clone(),
                rh._try_map(p, cb)?.swim(p),
            ),
            Expr::Squiggle(l, list, r) => Expr::Squiggle(
                l.clone(),
                list._try_map(p, cb)?.swim(p),
                r.clone(),
            ),
        };

        cb(p, expr)
    }
}


//// utils ///

// whitespace stuff
impl<'a> Tree<'a> {
    pub fn ws<S: Into<Cow<'a, str>>>(self, ws: S) -> Self {
        let mut first = true;
        let ws = ws.into();
        self.map_tokens(|_, tok| {
            if first {
                let tok = tok.ws(ws.clone());
                first = false;
                tok
            } else {
                tok
            }
        })
    }

    pub fn indent(self, n: usize) -> Self {
        self.ws(format!("\n{:n$}", "", n=n))
    }
}

impl<'a> Expr<'a> {
    pub fn ws<S: Into<Cow<'a, str>>>(self, o: &mut Pool<'a>, ws: S) -> Self {
        let mut first = true;
        let ws = ws.into();
        self.map(o, |_, tok: Token<'a>| {
            if first {
                let tok = tok.ws(ws.clone());
                first = false;
                tok
            } else {
                tok
            }
        })
    }

    pub fn indent(self, o: &mut Pool<'a>, n: usize) -> Self {
        self.ws(o, format!("\n{:n$}", "", n=n))
    }
}

// other traits
impl<'a> Clone for Tree<'a> {
    fn clone(&self) -> Self {
        self.map_tokens(|_, t| t)
    }
}

