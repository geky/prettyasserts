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
