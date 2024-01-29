#![allow(dead_code)]

use crate::tokenizer::Token;
use crate::tokenizer::Tt;
use crate::errors::ParseError;



// tree stuff
type List<'a> = Vec<(Option<Expr<'a>>, Option<Token<'a>>)>;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Symbol(Token<'a>),
    Number(Token<'a>),
    String(Token<'a>),
    Decl(Box<Expr<'a>>, Token<'a>),
    Call(Box<Expr<'a>>, Token<'a>, List<'a>, Token<'a>),
    Index(Box<Expr<'a>>, Token<'a>, List<'a>, Token<'a>),
    Neg(Token<'a>, Box<Expr<'a>>),
    AddrOf(Token<'a>, Box<Expr<'a>>),
    Dot(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Assign(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    BigArrow(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Squiggle(Token<'a>, List<'a>, Token<'a>),
}

#[derive(Debug, Clone)]
pub struct Tree<'a>(List<'a>, Option<Token<'a>>);


// helper for pattern matching
struct Parser<'a, 'b> {
    // input state
    tokens: &'b [Token<'a>],
    i: usize,
}

impl<'a, 'b> Parser<'a, 'b> {
    fn new(tokens: &'b [Token<'a>]) -> Self {
        Self{
            tokens: tokens,
            i: 0,
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
    // create parser
    let mut p = Parser::new(tokens);

    // define parse rules
    fn parse_expr<'a>(
        p: &mut Parser<'a, '_>
    ) -> Result<Option<Expr<'a>>, ParseError> {
        let mut lh = match p.tt() {
            Some(Tt::Symbol) => Expr::Symbol(p.munch()),
            Some(Tt::Number) => Expr::Number(p.munch()),
            Some(Tt::String) => Expr::String(p.munch()),
            Some(Tt::Sub) => Expr::Neg(
                p.munch(),
                Box::new(parse_expr_required(p)?),
            ),
            Some(Tt::And) => Expr::AddrOf(
                p.munch(),
                Box::new(parse_expr_required(p)?),
            ),
            Some(Tt::Dot) => Expr::AddrOf(
                p.munch(),
                Box::new(parse_expr_required(p)?),
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
                Some(Tt::Symbol) => Expr::Decl(Box::new(lh), p.munch()),
                Some(Tt::Dot) => Expr::Dot(
                    Box::new(lh),
                    p.munch(),
                    Box::new(parse_expr_required(p)?),
                ),
                Some(Tt::Eq) => Expr::Assign(
                    Box::new(lh),
                    p.munch(),
                    Box::new(parse_expr_required(p)?),
                ),
                Some(Tt::BigArrow) => Expr::BigArrow(
                    Box::new(lh),
                    p.munch(),
                    Box::new(parse_expr_required(p)?),
                ),
                Some(Tt::LParen) => Expr::Call(
                    Box::new(lh),
                    p.munch(),
                    parse_list(p)?,
                    match p.tt() {
                        Some(Tt::RParen) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ),
                Some(Tt::LSquare) => Expr::Index(
                    Box::new(lh),
                    p.munch(),
                    parse_list(p)?,
                    match p.tt() {
                        Some(Tt::RSquare) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ),
                _ => break,
            }
        }

        Ok(Some(lh))
    }

    fn parse_expr_required<'a>(
        p: &mut Parser<'a, '_>
    ) -> Result<Expr<'a>, ParseError> {
        Ok(match parse_expr(p)? {
            Some(expr) => expr,
            None => return Err(p.unexpected()),
        })
    }

    fn parse_list<'a>(
        p: &mut Parser<'a, '_>
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
                break list;
            }
        })
    }

    // start parsing
    let list = parse_list(&mut p)?;

    // just kind of shove any trailing whitespace into our tree
    let ws = if let Some(Tt::TrailingWs) = p.tt() {
        Some(p.munch())
    } else {
        None
    };

    // we should have consumed all tokens here
    if !p.is_done() {
        return Err(p.unexpected());
    }

    Ok(Tree(list, ws))
}


//// utils ///

// token traversal
impl<'a> Tree<'a> {
    pub fn visit_tokens<F>(&self, mut cb: F)
    where
        F: FnMut(&Token<'a>)
    {
        self.try_visit_tokens(|tok| Ok::<_,()>(cb(tok))).unwrap()
    }

    // hacky, expensive! but easier than rewritting all of these pesky
    // match statements
    pub fn try_visit_tokens<F, E>(&self, mut cb: F) -> Result<(), E>
    where
        F: FnMut(&Token<'a>) -> Result<(), E>
    {
        self.clone()._try_map_tokens(&mut |tok| {
            cb(&tok)?;
            Ok(tok)
        })?;
        Ok(())
    }

    pub fn map_tokens<F>(self, mut cb: F) -> Self
    where
        F: FnMut(Token<'a>) -> Token<'a>
    {
        self._try_map_tokens(&mut |tok| Ok::<_,()>(cb(tok))).unwrap()
    }

    pub fn try_map_tokens<F, E>(self, mut cb: F) -> Result<Self, E>
    where
        F: FnMut(Token<'a>) -> Result<Token<'a>, E>
    {
        self._try_map_tokens(&mut cb)
    }

    fn _try_map_tokens<E>(
        self,
        // this recursion makes the compiler explode if generic!
        mut cb: &mut dyn FnMut(Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self, E> {
        Ok(Tree(
            list_try_map_tokens(self.0, &mut cb)?,
            self.1.map(&mut cb).transpose()?,
        ))
    }
}

fn list_try_map_tokens<'a, E>(
    list: List<'a>,
    // this recursion makes the compiler explode if generic!
    mut cb: &mut dyn FnMut(Token<'a>) -> Result<Token<'a>, E>
) -> Result<List<'a>, E> {
    let mut list_ = vec![];
    for (expr, comma) in list.into_iter() {
        list_.push((
            match expr {
                Some(expr) => Some(expr._try_map_tokens(&mut cb)?),
                None => None,
            },
            comma.map(&mut cb).transpose()?
        ));
    }
    Ok(list_)
}

impl<'a> Expr<'a> {
    fn _try_map_tokens<E>(
        self,
        // this recursion makes the compiler explode if generic!
        mut cb: &mut dyn FnMut(Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self, E> {
        Ok(match self {
            Expr::Symbol(tok) => Expr::Symbol(cb(tok)?),
            Expr::Number(tok) => Expr::Number(cb(tok)?),
            Expr::String(tok) => Expr::String(cb(tok)?),
            Expr::Decl(expr, tok) => Expr::Decl(
                Box::new(expr._try_map_tokens(&mut cb)?),
                cb(tok)?,
            ),
            Expr::Call(expr, l, list, r) => Expr::Call(
                Box::new(expr._try_map_tokens(&mut cb)?),
                cb(l)?,
                list_try_map_tokens(list, &mut cb)?,
                cb(r)?,
            ),
            Expr::Index(expr, l, list, r) => Expr::Index(
                Box::new(expr._try_map_tokens(&mut cb)?),
                cb(l)?,
                list_try_map_tokens(list, &mut cb)?,
                cb(r)?,
            ),
            Expr::Neg(tok, expr) => Expr::Neg(
                cb(tok)?,
                Box::new(expr._try_map_tokens(&mut cb)?),
            ),
            Expr::AddrOf(tok, expr) => Expr::AddrOf(
                cb(tok)?,
                Box::new(expr._try_map_tokens(&mut cb)?),
            ),
            Expr::Dot(lh, tok, rh) => Expr::Dot(
                Box::new(lh._try_map_tokens(&mut cb)?),
                cb(tok)?,
                Box::new(rh._try_map_tokens(&mut cb)?),
            ),
            Expr::Assign(lh, tok, rh) => Expr::Assign(
                Box::new(lh._try_map_tokens(&mut cb)?),
                cb(tok)?,
                Box::new(rh._try_map_tokens(&mut cb)?),
            ),
            Expr::BigArrow(lh, tok, rh) => Expr::BigArrow(
                Box::new(lh._try_map_tokens(&mut cb)?),
                cb(tok)?,
                Box::new(rh._try_map_tokens(&mut cb)?),
            ),
            Expr::Squiggle(l, list, r) => Expr::Squiggle(
                cb(l)?,
                list_try_map_tokens(list, &mut cb)?,
                cb(r)?,
            ),
        })
    }
}


// expr traversal
impl<'a> Tree<'a> {
    pub fn visit_exprs<F>(&self, mut cb: F)
    where
        F: FnMut(&Expr<'a>)
    {
        self.try_visit_exprs(|expr| Ok::<_,()>(cb(expr))).unwrap()
    }

    // hacky, expensive! but easier than rewritting all of these pesky
    // match statements
    pub fn try_visit_exprs<F, E>(&self, mut cb: F) -> Result<(), E>
    where
        F: FnMut(&Expr<'a>) -> Result<(), E>
    {
        self.clone()._try_map_exprs(&mut |expr| {
            cb(&expr)?;
            Ok(expr)
        })?;
        Ok(())
    }

    pub fn map_exprs<F>(self, mut cb: F) -> Self
    where
        F: FnMut(Expr<'a>) -> Expr<'a>
    {
        self._try_map_exprs(&mut |expr| Ok::<_,()>(cb(expr))).unwrap()
    }

    pub fn try_map_exprs<F, E>(self, mut cb: F) -> Result<Self, E>
    where
        F: FnMut(Expr<'a>) -> Result<Expr<'a>, E>
    {
        self._try_map_exprs(&mut cb)
    }

    fn _try_map_exprs<E>(
        self,
        // this recursion makes the compiler explode if generic!
        mut cb: &mut dyn FnMut(Expr<'a>) -> Result<Expr<'a>, E>
    ) -> Result<Self, E> {
        Ok(Tree(
            list_try_map_exprs(self.0, &mut cb)?,
            self.1,
        ))
    }
}

fn list_try_map_exprs<'a, E>(
    list: List<'a>,
    // this recursion makes the compiler explode if generic!
    mut cb: &mut dyn FnMut(Expr<'a>) -> Result<Expr<'a>, E>
) -> Result<List<'a>, E> {
    let mut list_ = vec![];
    for (expr, comma) in list.into_iter() {
        list_.push((
            expr.map(&mut cb).transpose()?,
            comma
        ));
    }
    Ok(list_)
}


impl<'a> Expr<'a> {
    fn _try_map_exprs<E>(
        self,
        // this recursion makes the compiler explode if generic!
        mut cb: &mut dyn FnMut(Expr<'a>) -> Result<Expr<'a>, E>
    ) -> Result<Self, E> {
        // map bottom-up so we are always guaranteed to make progress
        let expr = match self {
            Expr::Symbol(tok) => Expr::Symbol(tok),
            Expr::Number(tok) => Expr::Number(tok),
            Expr::String(tok) => Expr::String(tok),
            Expr::Decl(expr, tok) => Expr::Decl(
                Box::new(expr._try_map_exprs(&mut cb)?),
                tok,
            ),
            Expr::Call(expr, l, list, r) => Expr::Call(
                Box::new(expr._try_map_exprs(&mut cb)?),
                l,
                list_try_map_exprs(list, &mut cb)?,
                r,
            ),
            Expr::Index(expr, l, list, r) => Expr::Index(
                Box::new(expr._try_map_exprs(&mut cb)?),
                l,
                list_try_map_exprs(list, &mut cb)?,
                r,
            ),
            Expr::Neg(tok, expr) => Expr::Neg(
                tok,
                Box::new(expr._try_map_exprs(&mut cb)?),
            ),
            Expr::AddrOf(tok, expr) => Expr::AddrOf(
                tok,
                Box::new(expr._try_map_exprs(&mut cb)?),
            ),
            Expr::Dot(lh, tok, rh) => Expr::Dot(
                Box::new(lh._try_map_exprs(&mut cb)?),
                tok,
                Box::new(rh._try_map_exprs(&mut cb)?),
            ),
            Expr::Assign(lh, tok, rh) => Expr::Assign(
                Box::new(lh._try_map_exprs(&mut cb)?),
                tok,
                Box::new(rh._try_map_exprs(&mut cb)?),
            ),
            Expr::BigArrow(lh, tok, rh) => Expr::BigArrow(
                Box::new(lh._try_map_exprs(&mut cb)?),
                tok,
                Box::new(rh._try_map_exprs(&mut cb)?),
            ),
            Expr::Squiggle(l, list, r) => Expr::Squiggle(
                l,
                list_try_map_exprs(list, &mut cb)?,
                r,
            ),
        };

        cb(expr)
    }
}
