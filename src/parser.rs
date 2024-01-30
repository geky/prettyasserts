#![allow(dead_code)]

use std::borrow::Cow;
use std::mem::transmute;
use std::cell::RefCell;

use crate::tokenizer::Token;
use crate::tokenizer::Tt;
use crate::errors::ParseError;



// tree stuff
#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Sym(Token<'a>),
    Lit(Token<'a>),
    Decl(&'a Expr<'a>, Token<'a>),
    Call(&'a Expr<'a>, Token<'a>, List<'a>, Token<'a>),
    Index(&'a Expr<'a>, Token<'a>, List<'a>, Token<'a>),
    Block(&'a Expr<'a>, Token<'a>, List<'a>, Token<'a>),
    Unary(Token<'a>, &'a Expr<'a>),
    Suffnary(&'a Expr<'a>, Token<'a>),
    Binary(&'a Expr<'a>, Token<'a>, &'a Expr<'a>),
    Ternary(&'a Expr<'a>, Token<'a>, &'a Expr<'a>, Token<'a>, &'a Expr<'a>),
    Squiggle(Token<'a>, List<'a>, Token<'a>),
}

type List<'a> = &'a [(Option<Expr<'a>>, Option<Token<'a>>)];


pub struct Tree<'a> {
    root: List<'a>,
    trailing_ws: Option<Token<'a>>,

    pool: Pool<'a>,
}

impl std::fmt::Debug for Tree<'_> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>
    ) -> Result<(), std::fmt::Error> {
        f.debug_struct("Tree")
            .field("root", &self.root)
            .field("trailing_ws", &self.trailing_ws)
            // ignore pool
            .finish()
    }
}

impl Clone for Tree<'_> {
    fn clone(&self) -> Self {
        todo!()
    }
}


// we really want pattern matching, unfortunately pattern matching
// boxes/derefs is currently todo in Rust, soooooooooo
//
// this Unpin is needed because we can't express what we want with
// Any, Any requires 'static
trait PoolBox: Unpin {}
impl<T: Unpin> PoolBox for T {}

trait PoolRef: PoolBox {
    fn pool<'a, P>(self, p: &mut P) -> &'a Self
    where
        P: AsMut<Pool<'a>>;
}

impl<T: PoolBox> PoolRef for T {
    fn pool<'a, P>(self, p: &mut P) -> &'a Self
    where
        P: AsMut<Pool<'a>>
    {
        p.as_mut().pool_box(self)
    }
}

pub struct Pool<'a>(Vec<Box<dyn PoolBox + 'a>>);

impl<'a> Pool<'a> {
    fn new() -> Self {
        Pool(vec![])
    }

    fn pool_box<T: PoolBox>(&mut self, t: T) -> &'a T {
        self.0.push(Box::new(t));
        let ref_ = self.0.last().unwrap().as_ref();
        unsafe { &*(ref_ as *const dyn PoolBox as *const T) }
    }
}

impl<'a> AsMut<Pool<'a>> for Pool<'a> {
    fn as_mut(&mut self) -> &mut Pool<'a> {
        self
    }
}

impl<'a> AsMut<Pool<'a>> for Tree<'a> {
    fn as_mut(&mut self) -> &mut Pool<'a> {
        &mut self.pool
    }
}



// helper for pattern matching
struct Parser<'b, 'a> {
    // input state
    tokens: &'b [Token<'a>],
    i: usize,

    // memory pool
    pool: Pool<'a>
}

impl<'b, 'a> Parser<'b, 'a> {
    fn new(tokens: &'b [Token<'a>]) -> Self {
        Self{
            tokens: tokens,
            i: 0,
            pool: Pool::new(),
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

impl<'a> AsMut<Pool<'a>> for Parser<'_, 'a> {
    fn as_mut(&mut self) -> &mut Pool<'a> {
        &mut self.pool
    }
}


// entry point
pub fn parse<'a>(tokens: &[Token<'a>]) -> Result<Tree<'a>, ParseError> {
    // create parser
    let mut p = Parser::new(tokens);

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
                parse_expr_required(p)?.pool(p),
            ),
            Some(Tt::Add) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.pool(p),
            ),
            Some(Tt::Sub) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.pool(p),
            ),
            Some(Tt::And) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.pool(p),
            ),
            Some(Tt::Dot) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.pool(p),
            ),
            Some(Tt::Not) => Expr::Unary(
                p.munch(),
                parse_expr_required(p)?.pool(p),
            ),
            Some(Tt::LParen) => Expr::Squiggle(
                p.munch(),
                parse_list(p)?,
                match p.tt() {
                    Some(Tt::RParen) => p.munch(),
                    _ => return Err(p.unexpected()),
                },
            ),
            Some(Tt::LSquiggle) => Expr::Squiggle(
                p.munch(),
                parse_list(p)?,
                match p.tt() {
                    Some(Tt::RSquiggle) => p.munch(),
                    _ => return Err(p.unexpected()),
                },
            ),
            _ => return Ok(None),
        };

        loop {
            lh = match p.tt() {
                Some(Tt::Sym) => Expr::Decl(lh.pool(p), p.munch()),
                Some(Tt::PlusPlus) => Expr::Suffnary(
                    lh.pool(p),
                    p.munch(),
                ),
                Some(Tt::Dot) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Splat) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Slash) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Mod) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Add) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Sub) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::And) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::OrOr) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Eq) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Ne) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Le) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Ge) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Lt) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Gt) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Assign) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::AddAssign) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::SubAssign) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Arrow) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::BigArrow) => Expr::Binary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::Question) => Expr::Ternary(
                    lh.pool(p),
                    p.munch(),
                    parse_expr_required(p)?.pool(p),
                    match p.tt() {
                        Some(Tt::Colon) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                    parse_expr_required(p)?.pool(p),
                ),
                Some(Tt::LParen) => Expr::Call(
                    lh.pool(p),
                    p.munch(),
                    parse_list(p)?,
                    match p.tt() {
                        Some(Tt::RParen) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ),
                Some(Tt::LSquare) => Expr::Index(
                    lh.pool(p),
                    p.munch(),
                    parse_list(p)?,
                    match p.tt() {
                        Some(Tt::RSquare) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ),
                Some(Tt::LSquiggle) => Expr::Block(
                    lh.pool(p),
                    p.munch(),
                    parse_list(p)?,
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
    ) -> Result<List<'a>, ParseError> {
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
                break list.pool(p);
            }
        })
    }

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

    Ok(Tree{
        root: root,
        trailing_ws: trailing_ws,
        pool: p.pool,
    })
}


////// utils ///
//
//// whitespace stuff
//impl<'a> Tree<'a> {
//    pub fn ws<S: Into<Cow<'a, str>>>(self, ws: S) -> Self {
//        let mut first = true;
//        let ws = ws.into();
//        self.map_tokens(|tok| {
//            if first {
//                let tok = tok.ws(ws.clone());
//                first = false;
//                tok
//            } else {
//                tok
//            }
//        })
//    }
//
//    pub fn indent(self, n: usize) -> Self {
//        self.ws(format!("\n{:n$}", "", n=n))
//    }
//}
//
//impl<'a> Expr<'a> {
//    pub fn ws<S: Into<Cow<'a, str>>>(self, ws: S) -> Self {
//        let mut first = true;
//        let ws = ws.into();
//        self.map_tokens(|tok| {
//            if first {
//                let tok = tok.ws(ws.clone());
//                first = false;
//                tok
//            } else {
//                tok
//            }
//        })
//    }
//
//    pub fn indent(self, n: usize) -> Self {
//        self.ws(format!("\n{:n$}", "", n=n))
//    }
//}


// token traversal
impl<'a> Tree<'a> {
    pub fn map_tokens<F>(self, mut cb: F) -> Self
    where
        F: FnMut(&mut Pool<'a>, Token<'a>) -> Token<'a>
    {
        self._try_map_tokens(&mut |p, tok| Ok::<_,()>(cb(p, tok))).unwrap()
    }

    pub fn try_map_tokens<F, E>(self, mut cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'a>, Token<'a>) -> Result<Token<'a>, E>
    {
        self._try_map_tokens(&mut cb)
    }

    fn _try_map_tokens<E>(
        self,
        // this recursion makes the compiler explode if generic!
        mut cb: &mut dyn FnMut(&mut Pool<'a>, Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self, E> {
        let Tree{root, trailing_ws, mut pool} = self;
        Ok(Tree{
            root: list_try_map_tokens(root, &mut pool, &mut cb)?,
            trailing_ws: trailing_ws.map(|tok| cb(&mut pool, tok)).transpose()?,
            pool: pool,
        })
    }
}

fn list_try_map_tokens<'a, E>(
    list: List<'a>,
    p: &mut Pool<'a>,
    // this recursion makes the compiler explode if generic!
    mut cb: &mut dyn FnMut(&mut Pool<'a>, Token<'a>) -> Result<Token<'a>, E>
) -> Result<List<'a>, E> {
    let mut list_ = vec![];
    for (expr, comma) in list.iter() {
        list_.push((
            expr.as_ref().map(|expr| expr._try_map_tokens(p, &mut cb)).transpose()?,
            comma.as_ref().map(|tok| cb(p, tok.clone())).transpose()?
        ));
    }
    Ok(list_.pool(p))
}

impl<'a> Expr<'a> {
    fn _try_map_tokens<E>(
        &self,
        p: &mut Pool<'a>,
        // this recursion makes the compiler explode if generic!
        mut cb: &mut dyn FnMut(&mut Pool<'a>, Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self, E> {
        Ok(match self {
            Expr::Sym(tok) => Expr::Sym(cb(p, tok.clone())?),
            Expr::Lit(tok) => Expr::Lit(cb(p, tok.clone())?),
            Expr::Decl(expr, tok) => Expr::Decl(
                expr._try_map_tokens(p, &mut cb)?.pool(p),
                cb(p, tok.clone())?,
            ),
            Expr::Call(expr, l, list, r) => Expr::Call(
                expr._try_map_tokens(p, &mut cb)?.pool(p),
                cb(p, l.clone())?,
                list_try_map_tokens(list, p, &mut cb)?,
                cb(p, r.clone())?,
            ),
            Expr::Index(expr, l, list, r) => Expr::Index(
                expr._try_map_tokens(p, &mut cb)?.pool(p),
                cb(p, l.clone())?,
                list_try_map_tokens(list, p, &mut cb)?,
                cb(p, r.clone())?,
            ),
            Expr::Block(expr, l, list, r) => Expr::Block(
                expr._try_map_tokens(p, &mut cb)?.pool(p),
                cb(p, l.clone())?,
                list_try_map_tokens(list, p, &mut cb)?,
                cb(p, r.clone())?,
            ),
            Expr::Unary(tok, expr) => Expr::Unary(
                cb(p, tok.clone())?,
                expr._try_map_tokens(p, &mut cb)?.pool(p),
            ),
            Expr::Suffnary(expr, tok) => Expr::Suffnary(
                expr._try_map_tokens(p, &mut cb)?.pool(p),
                cb(p, tok.clone())?,
            ),
            Expr::Binary(lh, tok, rh) => Expr::Binary(
                lh._try_map_tokens(p, &mut cb)?.pool(p),
                cb(p, tok.clone())?,
                rh._try_map_tokens(p, &mut cb)?.pool(p),
            ),
            Expr::Ternary(lh, l, mh, r, rh) => Expr::Ternary(
                lh._try_map_tokens(p, &mut cb)?.pool(p),
                cb(p, l.clone())?,
                mh._try_map_tokens(p, &mut cb)?.pool(p),
                cb(p, r.clone())?,
                rh._try_map_tokens(p, &mut cb)?.pool(p),
            ),
            Expr::Squiggle(l, list, r) => Expr::Squiggle(
                cb(p, l.clone())?,
                list_try_map_tokens(list, p, &mut cb)?,
                cb(p, r.clone())?,
            ),
        })
    }
}


// expr traversal
impl<'a> Tree<'a> {
    pub fn map_exprs<F>(self, mut cb: F) -> Self
    where
        F: FnMut(&mut Pool<'a>, Expr<'a>) -> Expr<'a>
    {
        self._try_map_exprs(&mut |p, expr| Ok::<_,()>(cb(p, expr))).unwrap()
    }

    pub fn try_map_exprs<F, E>(self, mut cb: F) -> Result<Self, E>
    where
        F: FnMut(&mut Pool<'a>, Expr<'a>) -> Result<Expr<'a>, E>
    {
        self._try_map_exprs(&mut cb)
    }

    fn _try_map_exprs<E>(
        self,
        // this recursion makes the compiler explode if generic!
        mut cb: &mut dyn FnMut(&mut Pool<'a>, Expr<'a>) -> Result<Expr<'a>, E>
    ) -> Result<Self, E> {
        let Tree{root, trailing_ws, mut pool} = self;
        Ok(Tree{
            root: list_try_map_exprs(root, &mut pool, &mut cb)?,
            trailing_ws: trailing_ws,
            pool: pool,
        })
    }
}

fn list_try_map_exprs<'a, E>(
    list: List<'a>,
    p: &mut Pool<'a>,
    // this recursion makes the compiler explode if generic!
    mut cb: &mut dyn FnMut(&mut Pool<'a>, Expr<'a>) -> Result<Expr<'a>, E>
) -> Result<List<'a>, E> {
    let mut list_ = vec![];
    for (expr, comma) in list.into_iter() {
        list_.push((
            expr.as_ref().map(|expr| expr._try_map_exprs(p, &mut cb)).transpose()?,
            comma.clone()
        ));
    }
    Ok(list_.pool(p))
}

impl<'a> Expr<'a> {
    fn _try_map_exprs<E>(
        &self,
        p: &mut Pool<'a>,
        // this recursion makes the compiler explode if generic!
        mut cb: &mut dyn FnMut(&mut Pool<'a>, Expr<'a>) -> Result<Expr<'a>, E>
    ) -> Result<Self, E> {
        // map bottom-up so we are always guaranteed to make progress
        let expr = match self {
            Expr::Sym(tok) => Expr::Sym(tok.clone()),
            Expr::Lit(tok) => Expr::Lit(tok.clone()),
            Expr::Decl(expr, tok) => Expr::Decl(
                expr._try_map_exprs(p, &mut cb)?.pool(p),
                tok.clone(),
            ),
            Expr::Call(expr, l, list, r) => Expr::Call(
                expr._try_map_exprs(p, &mut cb)?.pool(p),
                l.clone(),
                list_try_map_exprs(list, p, &mut cb)?,
                r.clone(),
            ),
            Expr::Index(expr, l, list, r) => Expr::Index(
                expr._try_map_exprs(p, &mut cb)?.pool(p),
                l.clone(),
                list_try_map_exprs(list, p, &mut cb)?,
                r.clone(),
            ),
            Expr::Block(expr, l, list, r) => Expr::Block(
                expr._try_map_exprs(p, &mut cb)?.pool(p),
                l.clone(),
                list_try_map_exprs(list, p, &mut cb)?,
                r.clone(),
            ),
            Expr::Unary(tok, expr) => Expr::Unary(
                tok.clone(),
                expr._try_map_exprs(p, &mut cb)?.pool(p),
            ),
            Expr::Suffnary(expr, tok) => Expr::Suffnary(
                expr._try_map_exprs(p, &mut cb)?.pool(p),
                tok.clone(),
            ),
            Expr::Binary(lh, tok, rh) => Expr::Binary(
                lh._try_map_exprs(p, &mut cb)?.pool(p),
                tok.clone(),
                rh._try_map_exprs(p, &mut cb)?.pool(p),
            ),
            Expr::Ternary(lh, l, mh, r, rh) => Expr::Ternary(
                lh._try_map_exprs(p, &mut cb)?.pool(p),
                l.clone(),
                mh._try_map_exprs(p, &mut cb)?.pool(p),
                r.clone(),
                rh._try_map_exprs(p, &mut cb)?.pool(p),
            ),
            Expr::Squiggle(l, list, r) => Expr::Squiggle(
                l.clone(),
                list_try_map_exprs(list, p, &mut cb)?,
                r.clone(),
            ),
        };

        cb(p, expr)
    }
}
