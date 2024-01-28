#![allow(dead_code)]

use regex::Regex;

use std::collections::HashMap;
use std::cmp::min;
use std::path::PathBuf;
use std::rc::Rc;

use crate::errors::ParseError;


// lexical stuff
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Tt {
    Symbol,     // blah1
    LParen,     // (
    RParen,     // )
    LSquiggle,  // {
    RSquiggle,  // }
    LSquare,    // [
    RSquare,    // ]
    EqEq,       // ==
    Eq,         // =
    And,        // &
    Splat,      // *
    Comma,      // ,
    Semi,       // ;
    TrailingWs,
}

#[derive(Clone)]
pub struct Token<'a> {
    file: Rc<PathBuf>,
    line: usize,
    col: usize,
    tt: Tt,
    ws: &'a str,
    tok: &'a str,
}

impl std::fmt::Debug for Token<'_> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>
    ) -> Result<(), std::fmt::Error> {
        write!(f, "Token[{}:{}:{}: {:?} {:?}]",
            self.file.display(),
            self.line,
            self.col,
            self.tt,
            self.tok
        )
    }
}

// helper for pattern matching
struct Matcher<'a> {
    file: Rc<PathBuf>,
    line: usize,
    col: usize,

    // input state
    input: &'a str,
    i: usize,
    ws_i: usize,

    // cache for regex
    cache: HashMap<&'static str, Regex>,
    // last match
    found: &'a str,
}

impl<'a> Matcher<'a> {
    fn new(file: &Rc<PathBuf>, input: &'a str) -> Self {
        Self{
            file: file.clone(),
            line: 1,
            col: 1,
            input: input,
            i: 0,
            ws_i: 0,

            cache: HashMap::new(),
            found: "",
        }
    }

    fn is_done(&self) -> bool {
        self.i >= self.input.len()
    }

    fn tail(&self) -> &'a str {
        &self.input[self.i..]
    }

    fn _next(&mut self, n: usize) {
        for _ in 0..n {
            if self.input[self.i..].chars().next() == Some('\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.i += 1;
        }
    }

    fn match_(&mut self, p: &'static str) -> Option<&'a str> {
        let r = self.cache.entry(p)
            .or_insert_with(|| {
                // we need to inject a ^ because of course we do, at least
                // this gets cached
                let p = format!("^{}", p);
                Regex::new(&p).unwrap()
            });
        let found = r.find(&self.input[self.i..]).map(|m| m.as_str());
        // save the last match
        self.found = found.unwrap_or("");
        found
    }

    fn matches(&mut self, p: &'static str) -> bool {
        self.match_(p).is_some()
    }

    fn skip_ws(&mut self, n: usize) {
        self._next(n)
    }

    fn munch(&mut self, tt: Tt) -> Token<'a> {
        let tok = Token {
            file: self.file.clone(),
            line: self.line,
            col: self.col,
            tt: tt,
            ws: &self.input[self.ws_i..self.i],
            tok: self.found,
        };
        self._next(tok.tok.len());
        self.ws_i = self.i;
        self.found = "";
        tok
    }

    fn error(&self, message: String) -> ParseError {
        ParseError::new(&self.file, self.line, self.col, message)
    }
}

// tokenizer
pub fn tokenize<'a>(
    file: &Rc<PathBuf>,
    input: &'a str
) -> Result<Vec<Token<'a>>, ParseError> {
    let mut tokens = vec![];
    let mut m = Matcher::new(file, input);

    while !m.is_done() {
        // parse whitespace separately
        while !m.is_done() {
            match m.tail() {
                _ if m.matches(r" ")  => m.skip_ws(1),
                _ if m.matches(r"\n") => m.skip_ws(1),
                _ => break,
            }
        }

        // now parse the actual token
        tokens.push(match m.tail() {
            // symbols
            _ if m.matches(r"[a-zA-Z_][a-zA-Z_0-9]*") => {
                m.munch(Tt::Symbol)
            },
            // tokens
            _ if m.matches(r"\(") => m.munch(Tt::LParen),
            _ if m.matches(r"\)") => m.munch(Tt::RParen),
            _ if m.matches(r"\{") => m.munch(Tt::LSquiggle),
            _ if m.matches(r"\}") => m.munch(Tt::RSquiggle),
            _ if m.matches(r"\[") => m.munch(Tt::LSquare),
            _ if m.matches(r"\]") => m.munch(Tt::RSquare),
            _ if m.matches(r"==") => m.munch(Tt::EqEq),
            _ if m.matches(r"=")  => m.munch(Tt::Eq),
            _ if m.matches(r"&")  => m.munch(Tt::And),
            _ if m.matches(r"\*") => m.munch(Tt::Splat),
            _ if m.matches(r",") => m.munch(Tt::Comma),
            _ if m.matches(r";") => m.munch(Tt::Semi),
            // wait, end of input?
            "" => m.munch(Tt::TrailingWs),
            // unknown token
            tail => {
                return Err(m.error(format!(
                    "Unknown token: \"{}...\"",
                    &tail[..min(tail.len(), 8)]
                )));
            }
        });
    }

    Ok(tokens)
}
