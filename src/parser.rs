
use std::borrow::Cow;
use std::path::Path;

use crate::tokenizer::Token;
use crate::tokenizer::Tt;
use crate::errors::ParseError;
use crate::rc::Rc;
use crate::rc::Transmute;
use crate::rc::Transborrow;


// tree stuff
#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Sym(Token<'a>),
    Lit(Token<'a>),
    Label(Token<'a>, Token<'a>, Option<Rc<Expr<'a>>>),
    Decl(Rc<Expr<'a>>, Token<'a>),
    Return(Token<'a>, Option<Rc<Expr<'a>>>),
    Call(Rc<Expr<'a>>, Token<'a>, List<'a>, Token<'a>),
    Index(Rc<Expr<'a>>, Token<'a>, List<'a>, Token<'a>),
    Block(Rc<Expr<'a>>, Token<'a>, List<'a>, Token<'a>),
    Unary(Token<'a>, Rc<Expr<'a>>),
    Suffnary(Rc<Expr<'a>>, Token<'a>),
    Binary(Rc<Expr<'a>>, Token<'a>, Rc<Expr<'a>>),
    Ternary(Rc<Expr<'a>>, Token<'a>, Rc<Expr<'a>>, Token<'a>, Rc<Expr<'a>>),
    Squiggle(Token<'a>, List<'a>, Token<'a>),
}

type List<'a> = Rc<[(Option<Expr<'a>>, Option<Token<'a>>)]>;

#[derive(Debug, Clone)]
pub struct Tree<'a>(List<'a>, Token<'a>);

#[derive(Debug, Clone, Copy)]
pub enum Expr_<'b, 'a> {
    Sym(Token<'a>),
    Lit(Token<'a>),
    Label(Token<'a>, Token<'a>, Option<&'b Expr_<'b, 'a>>),
    Decl(&'b Expr_<'b, 'a>, Token<'a>),
    Return(Token<'a>, Option<&'b Expr_<'b, 'a>>),
    Call(&'b Expr_<'b, 'a>, Token<'a>, List_<'b, 'a>, Token<'a>),
    Index(&'b Expr_<'b, 'a>, Token<'a>, List_<'b, 'a>, Token<'a>),
    Block(&'b Expr_<'b, 'a>, Token<'a>, List_<'b, 'a>, Token<'a>),
    Unary(Token<'a>, &'b Expr_<'b, 'a>),
    Suffnary(&'b Expr_<'b, 'a>, Token<'a>),
    Binary(&'b Expr_<'b, 'a>, Token<'a>, &'b Expr_<'b, 'a>),
    Ternary(&'b Expr_<'b, 'a>, Token<'a>, &'b Expr_<'b, 'a>, Token<'a>, &'b Expr_<'b, 'a>),
    Squiggle(Token<'a>, List_<'b, 'a>, Token<'a>),
}

type List_<'b, 'a> = &'b [(Option<Expr_<'b, 'a>>, Option<Token<'a>>)];

#[derive(Debug, Clone, Copy)]
pub struct Tree_<'b, 'a>(List_<'b, 'a>, Token<'a>);

unsafe impl<'b, 'a> Transmute<Expr_<'b, 'a>> for Expr<'a> {}
unsafe impl<'b, 'a> Transmute<List_<'b, 'a>> for List<'a> {}
unsafe impl<'b, 'a> Transmute<Tree_<'b, 'a>> for Tree<'a> {}


// helper for pattern matching
struct Parser<'b, 'a> {
    // input state
    tokens: &'b [Token<'a>],
    i: usize,
}

impl<'b, 'a> Parser<'b, 'a> {
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

    fn tok(&self) -> Option<&'a str> {
        self.tokens.get(self.i).map(|tok| tok.tok)
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
    // create parser
    let mut p = Parser::new(tokens);

    // define parse rules
    fn parse_expr<'b, 'a>(
        p: &mut Parser<'b, 'a>
    ) -> Result<Option<Expr<'a>>, ParseError> {
        let mut lh = match p.tt() {
            Some(Tt::Sym) if p.tok() == Some("return") => Expr::Return(
                p.munch(),
                parse_expr(p)?.map(Rc::new)
            ),
            Some(Tt::Sym) => Expr::Sym(p.munch()),
            Some(
                Tt::Number
                    | Tt::String
            ) => Expr::Lit(p.munch()),
            Some(
                Tt::And
                    | Tt::Tilde
                    | Tt::Splat
                    | Tt::Add
                    | Tt::Sub
                    | Tt::Not
                    | Tt::Dot
            ) => Expr::Unary(
                p.munch(),
                Rc::new(parse_expr_required(p)?),
            ),
            Some(Tt::LParen) => Expr::Squiggle(
                p.munch(),
                parse_list(p)?,
                match p.tt() {
                    Some(Tt::RParen) => p.munch(),
                    _ => return Err(p.unexpected()),
                },
            ),
            // squiggles terminate
            Some(Tt::LSquiggle) => return Ok(Some(Expr::Squiggle(
                p.munch(),
                parse_list(p)?,
                match p.tt() {
                    Some(Tt::RSquiggle) => p.munch(),
                    // C ifdef mess unfortunately break squiggle matching
                    // pretty badly, so just accept unbalanced squiggles for
                    // now, but use an empty string to round-trip correctly
                    _ => tok(""),
                },
            ))),
            _ => return Ok(None),
        };

        loop {
            lh = match p.tt() {
                Some(
                    Tt::Sym
                        | Tt::String
                ) => Expr::Decl(Rc::new(lh), p.munch()),
                Some(
                    Tt::PlusPlus
                        | Tt::MinusMinus
                ) => Expr::Suffnary(
                    Rc::new(lh),
                    p.munch(),
                ),
                Some(Tt::Splat) => {
                    let tok = p.munch();
                    match parse_expr(p)? {
                        Some(rh) => Expr::Binary(
                            Rc::new(lh),
                            tok,
                            Rc::new(rh),
                        ),
                        None => Expr::Suffnary(
                            Rc::new(lh),
                            tok,
                        ),
                    }
                }
                Some(
                    Tt::BigArrow
                        | Tt::Arrow
                        | Tt::Shl
                        | Tt::Shr
                        | Tt::Eq
                        | Tt::Ne
                        | Tt::AddAssign
                        | Tt::SubAssign
                        | Tt::AndAssign
                        | Tt::OrAssign
                        | Tt::XorAssign
                        | Tt::Le
                        | Tt::Ge
                        | Tt::Lt
                        | Tt::Gt
                        | Tt::Assign
                        | Tt::AndAnd
                        | Tt::And
                        | Tt::OrOr
                        | Tt::Or
                        | Tt::Xor
                        | Tt::Slash
                        | Tt::Mod
                        | Tt::Add
                        | Tt::Sub
                        | Tt::Not
                        | Tt::Dot
                ) => Expr::Binary(
                    Rc::new(lh),
                    p.munch(),
                    Rc::new(parse_expr_required(p)?),
                ),
                Some(Tt::Question) => Expr::Ternary(
                    Rc::new(lh),
                    p.munch(),
                    Rc::new(parse_expr_required(p)?),
                    match p.tt() {
                        Some(Tt::Colon) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                    Rc::new(parse_expr_required(p)?),
                ),
                Some(Tt::LParen) => Expr::Call(
                    Rc::new(lh),
                    p.munch(),
                    parse_list(p)?,
                    match p.tt() {
                        Some(Tt::RParen) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ),
                Some(Tt::LSquare) => Expr::Index(
                    Rc::new(lh),
                    p.munch(),
                    parse_list(p)?,
                    match p.tt() {
                        Some(Tt::RSquare) => p.munch(),
                        _ => return Err(p.unexpected()),
                    },
                ),
                // squiggles terminate
                Some(Tt::LSquiggle) => return Ok(Some(Expr::Block(
                    Rc::new(lh),
                    p.munch(),
                    parse_list(p)?,
                    match p.tt() {
                        Some(Tt::RSquiggle) => p.munch(),
                        // C ifdef mess unfortunately break squiggle matching
                        // pretty badly, so just accept unbalanced squiggles for
                        // now, but use an empty string to round-trip correctly
                        _ => tok(""),
                    },
                ))),
                _ => break,
            }
        }

        Ok(Some(lh))
    }

    fn parse_expr_required<'b, 'a>(
        p: &mut Parser<'b, 'a>
    ) -> Result<Expr<'a>, ParseError> {
        Ok(match parse_expr(p)? {
            Some(expr) => expr,
            None => return Err(p.unexpected()),
        })
    }

    fn parse_stmt<'b, 'a>(
        p: &mut Parser<'b, 'a>
    ) -> Result<Option<Expr<'a>>, ParseError> {
        Ok(match p.tt() {
            Some(Tt::Sym)
                if p.tok() == Some("return")
            => Some(Expr::Return(
                p.munch(),
                parse_expr(p)?.map(Rc::new)
            )),
            Some(Tt::Sym)
                if p.tail().len() >= 2
                && p.tail()[1].tt == Tt::Colon
            => Some(Expr::Label(
                p.munch(),
                p.munch(),
                parse_expr(p)?.map(Rc::new)
            )),
            _ => parse_expr(p)?,
        })
    }

    fn parse_list<'b, 'a>(
        p: &mut Parser<'b, 'a>
    ) -> Result<List<'a>, ParseError> {
        let mut list = vec![];
        Ok(loop {
            let stmt = parse_stmt(p)?;
            let comma = match p.tt() {
                Some(Tt::Semi)  => Some(p.munch()),
                Some(Tt::Comma) => Some(p.munch()),
                _               => None,
            };

            match (&stmt, &comma) {
                (Some(Expr::Squiggle(..)), _)
                    | (Some(Expr::Block(..)), _)
                    | (Some(Expr::Label(..)), _)
                    | (_, Some(_))
                => {
                    list.push((stmt, comma));
                    continue;
                },
                (Some(_), _) => {
                    list.push((stmt, comma));
                }
                (None, None) => {}
            }
            break list.into();
        })
    }

    // start parsing
    let root = parse_list(&mut p)?;

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

    Ok(Tree(root, tws))
}


//// traversals ////

// it's complicated, but this combined traversal impl means we only need to
// actually navigate the tree structure once
#[derive(Debug, Clone)]
pub enum Fork<'a> {
    Expr(Expr<'a>),
    Token(Token<'a>),
}

// core traversal trait
pub trait Map<T>
where
    Self: Sized
{
    type Target;

    fn _try_map<E>(
        &self,
        // recursion makes the compiler explode if f is generic!
        cb: &mut dyn FnMut(T) -> Result<T, E>
    ) -> Result<Self::Target, E>;

    fn _map(
        &self,
        // recursion makes the compiler explode if f is generic!
        cb: &mut dyn FnMut(T) -> T
    ) -> Self::Target {
        self._try_map(&mut |t| Ok::<_, ()>(cb(t))).unwrap()
    }

    fn _try_visit<E>(
        &self,
        // recursion makes the compiler explode if f is generic!
        cb: &mut dyn FnMut(&T) -> Result<(), E>
    ) -> Result<(), E>
    where
        Self: Clone
    {
        self.clone()._try_map(&mut |t| cb(&t).map(|_| t))?;
        Ok(())
    }

    fn _visit(
        &self,
        // recursion makes the compiler explode if f is generic!
        cb: &mut dyn FnMut(&T)
    )
    where
        Self: Clone
    {
        self.clone()._try_visit(&mut |t| Ok::<_, ()>(cb(t))).unwrap()
    }

    // convenience wrappers, but if recursion is used use the above or
    // else Rust will barf when the compiler itself overflows
    fn try_map<F, E>(&self, mut cb: F) -> Result<Self::Target, E>
    where
        F: FnMut(T) -> Result<T, E>
    {
        self._try_map(&mut cb)
    }

    fn map<F>(&self, mut cb: F) -> Self::Target
    where
        F: FnMut(T) -> T
    {
        self._map(&mut cb)
    }
    
    fn try_visit<F, E>(&self, mut cb: F) -> Result<(), E>
    where
        F: FnMut(&T) -> Result<(), E>,
        Self: Clone
    {
        self._try_visit(&mut cb)
    }

    fn visit<F>(&self, mut cb: F)
    where
        F: FnMut(&T),
        Self: Clone
    {
        self._visit(&mut cb)
    }
}

// traversal impls
impl<'a> Map<Fork<'a>> for Token<'a> {
    type Target = Token<'a>;

    fn _try_map<E>(
        &self,
        cb: &mut dyn FnMut(Fork<'a>) -> Result<Fork<'a>, E>
    ) -> Result<Self::Target, E> {
        Ok(match cb(Fork::Token(*self))? {
            Fork::Token(self_) => self_,
            _ => unreachable!(),
        })
    }
}

impl<'b, 'a> Map<Fork<'a>> for Expr_<'b, 'a> {
    type Target = Expr<'a>;

    fn _try_map<E>(
        &self,
        cb: &mut dyn FnMut(Fork<'a>) -> Result<Fork<'a>, E>
    ) -> Result<Self::Target, E> {
        // map bottom-up so we are always guaranteed to make progress
        let expr = match self {
            Expr_::Sym(tok) => Expr::Sym(tok._try_map(cb)?),
            Expr_::Lit(tok) => Expr::Lit(tok._try_map(cb)?),
            Expr_::Label(label, tok, expr) => Expr::Label(
                label._try_map(cb)?,
                tok._try_map(cb)?,
                expr.map(|expr| expr._try_map(cb)).transpose()?.map(Rc::new),
            ),
            Expr_::Decl(expr, tok) => Expr::Decl(
                Rc::new(expr._try_map(cb)?),
                tok._try_map(cb)?,
            ),
            Expr_::Return(tok, expr) => Expr::Return(
                tok._try_map(cb)?,
                expr.map(|expr| expr._try_map(cb)).transpose()?.map(Rc::new),
            ),
            Expr_::Call(expr, l, list, r) => Expr::Call(
                Rc::new(expr._try_map(cb)?),
                l._try_map(cb)?,
                list._try_map(cb)?,
                r._try_map(cb)?,
            ),
            Expr_::Index(expr, l, list, r) => Expr::Index(
                Rc::new(expr._try_map(cb)?),
                l._try_map(cb)?,
                list._try_map(cb)?,
                r._try_map(cb)?,
            ),
            Expr_::Block(expr, l, list, r) => Expr::Block(
                Rc::new(expr._try_map(cb)?),
                l._try_map(cb)?,
                list._try_map(cb)?,
                r._try_map(cb)?,
            ),
            Expr_::Unary(tok, expr) => Expr::Unary(
                tok._try_map(cb)?,
                Rc::new(expr._try_map(cb)?),
            ),
            Expr_::Suffnary(expr, tok) => Expr::Suffnary(
                Rc::new(expr._try_map(cb)?),
                tok._try_map(cb)?,
            ),
            Expr_::Binary(lh, tok, rh) => Expr::Binary(
                Rc::new(lh._try_map(cb)?),
                tok._try_map(cb)?,
                Rc::new(rh._try_map(cb)?),
            ),
            Expr_::Ternary(lh, l, mh, r, rh) => Expr::Ternary(
                Rc::new(lh._try_map(cb)?),
                l._try_map(cb)?,
                Rc::new(mh._try_map(cb)?),
                r._try_map(cb)?,
                Rc::new(rh._try_map(cb)?),
            ),
            Expr_::Squiggle(l, list, r) => Expr::Squiggle(
                l._try_map(cb)?,
                list._try_map(cb)?,
                r._try_map(cb)?,
            ),
        };

        Ok(match cb(Fork::Expr(expr))? {
            Fork::Expr(self_) => self_,
            _ => unreachable!(),
        })
    }
}

impl<'b, 'a> Map<Fork<'a>> for List_<'b, 'a> {
    type Target = List<'a>;

    fn _try_map<E>(
        &self,
        cb: &mut dyn FnMut(Fork<'a>) -> Result<Fork<'a>, E>
    ) -> Result<Self::Target, E> {
        let mut list_ = vec![];
        for (expr, comma) in self.iter() {
            list_.push((
                expr.clone().map(|expr| expr._try_map(cb)).transpose()?,
                comma.map(|comma| comma._try_map(cb)).transpose()?
            ));
        }
        Ok(list_.into())
    }
}

impl<'b, 'a> Map<Fork<'a>> for Tree_<'b, 'a> {
    type Target = Tree<'a>;

    fn _try_map<E>(
        &self,
        cb: &mut dyn FnMut(Fork<'a>) -> Result<Fork<'a>, E>
    ) -> Result<Self::Target, E> {
        Ok(Tree(
            self.0._try_map(cb)?,
            self.1._try_map(cb)?
        ))
    }
}

// now we can split the fork
impl<'a, T> Map<Token<'a>> for T
where
    T: Map<Fork<'a>>
{
    type Target = <T as Map<Fork<'a>>>::Target;

    fn _try_map<E>(
        &self,
        cb: &mut dyn FnMut(Token<'a>) -> Result<Token<'a>, E>
    ) -> Result<Self::Target, E> {
        self._try_map(&mut |fork| {
            Ok(match fork {
                Fork::Token(tok) => Fork::Token(cb(tok)?),
                fork => fork,
            })
        })
    }
}

impl<'a, T> Map<Expr<'a>> for T
where
    T: Map<Fork<'a>>
{
    type Target = <T as Map<Fork<'a>>>::Target;

    fn _try_map<E>(
        &self,
        cb: &mut dyn FnMut(Expr<'a>) -> Result<Expr<'a>, E>
    ) -> Result<Self::Target, E> {
        self._try_map(&mut |fork| {
            Ok(match fork {
                Fork::Expr(expr) => Fork::Expr(cb(expr)?),
                fork => fork,
            })
        })
    }
}

// extend to tracking types
impl<'a> Map<Fork<'a>> for Expr<'a> {
    type Target = Expr<'a>;

    fn _try_map<E>(
        &self,
        cb: &mut dyn FnMut(Fork<'a>) -> Result<Fork<'a>, E>
    ) -> Result<Self::Target, E> {
        let self_: &Expr_<'_, 'a> = self.borrow();
        self_._try_map(cb)
    }
}

impl<'a> Map<Fork<'a>> for List<'a> {
    type Target = List<'a>;

    fn _try_map<E>(
        &self,
        cb: &mut dyn FnMut(Fork<'a>) -> Result<Fork<'a>, E>
    ) -> Result<Self::Target, E> {
        let self_: &List_<'_, 'a> = self.borrow();
        self_._try_map(cb)
    }
}

impl<'a> Map<Fork<'a>> for Tree<'a> {
    type Target = Tree<'a>;

    fn _try_map<E>(
        &self,
        cb: &mut dyn FnMut(Fork<'a>) -> Result<Fork<'a>, E>
    ) -> Result<Self::Target, E> {
        let self_: &Tree_<'_, 'a> = self.borrow();
        self_._try_map(cb)
    }
}

// from impls, thought these risks being a bit expensive
impl<'b, 'a> From<Expr_<'b, 'a>> for Expr<'a> {
    fn from(self_: Expr_<'b, 'a>) -> Self {
        self_.map(|t: Token<'a>| t)
    }
}

impl<'b, 'a> From<List_<'b, 'a>> for List<'a> {
    fn from(self_: List_<'b, 'a>) -> Self {
        self_.map(|t: Token<'a>| t)
    }
}

impl<'b, 'a> From<Tree_<'b, 'a>> for Tree<'a> {
    fn from(self_: Tree_<'b, 'a>) -> Self {
        self_.map(|t: Token<'a>| t)
    }
}

impl<'b, 'a> From<&Expr_<'b, 'a>> for Expr<'a> {
    fn from(self_: &Expr_<'b, 'a>) -> Self {
        self_.map(|t: Token<'a>| t)
    }
}

impl<'b, 'a> From<&List_<'b, 'a>> for List<'a> {
    fn from(self_: &List_<'b, 'a>) -> Self {
        self_.map(|t: Token<'a>| t)
    }
}

impl<'b, 'a> From<&Tree_<'b, 'a>> for Tree<'a> {
    fn from(self_: &Tree_<'b, 'a>) -> Self {
        self_.map(|t: Token<'a>| t)
    }
}


//// utils ///

// whitespace stuff
impl<'b, 'a> Expr_<'b, 'a> {
    pub fn file(&self) -> &'a Path {
        self.try_visit(|tok: &Token<'a>| Err(tok.file)).unwrap_err()
    }

    pub fn line(&self) -> usize {
        self.try_visit(|tok: &Token<'a>| Err(tok.line)).unwrap_err()
    }

    pub fn col(&self) -> usize {
        self.try_visit(|tok: &Token<'a>| Err(tok.col)).unwrap_err()
    }

    pub fn lws(&self) -> &'a str {
        self.try_visit(|tok: &Token<'a>| Err(tok.lws)).unwrap_err()
    }
}

impl<'a> Expr<'a> {
    pub fn file(&self) -> &'a Path {
        self.borrow().file()
    }

    pub fn line(&self) -> usize {
        self.borrow().line()
    }

    pub fn col(&self) -> usize {
        self.borrow().col()
    }

    pub fn lws(&self) -> &'a str {
        self.borrow().lws()
    }

    pub fn file_(&self, file: &'a Path) -> Self {
        self.map(|tok: Token<'a>| tok.file_(file))
    }

    pub fn line_(self, line: usize) -> Self {
        self.map(|tok: Token<'a>| tok.line_(line))
    }

    pub fn col_(self, col: usize) -> Self {
        self.map(|tok: Token<'a>| tok.col_(col))
    }

    pub fn lws_(self, lws: &'a str) -> Self {
        let mut first = true;
        self.map(|tok: Token<'a>| {
            let tok = if first { tok.lws_(lws) } else { tok };
            first = false;
            tok
        })
    }

    pub fn indent(self, n: usize) -> Self {
        let mut first = true;
        self.map(|tok: Token<'a>| {
            let tok = if first { tok.indent(n) } else { tok };
            first = false;
            tok
        })
    }
}


// extra utils, just to make tree creation easy
use crate::tokenizer::tok;

pub fn sym<'a, S: Into<Cow<'a, str>>>(s: S) -> Expr<'a> {
    Expr::Sym(tok(s))
}

// a span is sort of an invisible squiggle, usually for injecting multiple
// exprs into a single expr
pub fn span<'a, L: AsRef<[Expr<'a>]>>(list: L) -> Expr<'a> {
    let list = list.as_ref();
    let mut list_ = vec![];
    for (i, expr) in list.iter().enumerate() {
        list_.push((
            Some(expr.clone()),
            if i < list.len()-1 {
                Some(tok(";"))
            } else {
                None
            }
        ))
    }

    Expr::Squiggle(
        tok(""),
        list_.into(),
        tok(""),
    )
}


