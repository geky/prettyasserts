#![allow(dead_code)]

use regex::Regex;

use std::collections::HashMap;
use std::cmp::min;
use std::path::PathBuf;
use std::rc::Rc;
use std::borrow::Cow;

use crate::errors::ParseError;


// lexical stuff
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Tt {
    Sym,        // blah1
    Number,     // 0x1234
    String,     // "asdfasdf"
    LParen,     // (
    RParen,     // )
    LSquiggle,  // {
    RSquiggle,  // }
    LSquare,    // [
    RSquare,    // ]
    BigArrow,   // =>
    Eq,         // ==
    Assign,     // =
    Dot,        // .
    And,        // &
    Splat,      // *
    Arrow,      // ->
    Sub,        // -
    Comma,      // ,
    Semi,       // ;
    TrailingWs,
}

#[derive(Clone)]
pub struct Token<'a> {
    pub file: Rc<PathBuf>,
    pub line: usize,
    pub col: usize,
    pub tt: Tt,
    pub ws: Cow<'a, str>,
    pub tok: Cow<'a, str>,
}

impl<'a> Token<'a> {
    pub fn new<S: Into<Cow<'a, str>>>(tt: Tt, tok: S) -> Self {
        Self{
            file: Rc::new(PathBuf::new()),
            line: 1,
            col: 1,
            tt: tt,
            ws: Cow::from(""),
            tok: tok.into(),
        }
    }

    pub fn ws<S: Into<Cow<'a, str>>>(self, ws: S) -> Self {
        Self{
            ws: ws.into(),
            ..self
        }
    }

    pub fn indent(self, n: usize) -> Self {
        self.ws(format!("\n{:n$}", "", n=n))
    }
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
struct Tokenizer<'a> {
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

impl<'a> Tokenizer<'a> {
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
        if let Some(found) = found {
            self.found = found;
        }
        found
    }

    fn matches(&mut self, p: &'static str) -> bool {
        self.match_(p).is_some()
    }

    fn skip_ws(&mut self, n: usize) {
        self._next(n);
        self.found = "";
    }

    fn munch(&mut self, tt: Tt) -> Token<'a> {
        let tok = Token {
            file: self.file.clone(),
            line: self.line,
            col: self.col,
            tt: tt,
            ws: Cow::from(&self.input[self.ws_i..self.i]),
            tok: Cow::from(self.found),
        };
        self._next(tok.tok.len());
        self.ws_i = self.i;
        self.found = "";
        tok
    }

    fn error(&self, message: String) -> ParseError {
        ParseError::new(&self.file, self.line, self.col, message)
    }

    fn unknown(&self) -> ParseError {
        let tail = self.tail();
        self.error(format!(
            "Unknown token: \"{}...\"",
            &tail[..min(tail.len(), 8)]
        ))
    }
}

// tokenizer
pub fn tokenize<'a>(
    file: &Rc<PathBuf>,
    input: &'a str
) -> Result<Vec<Token<'a>>, ParseError> {
    let mut tokens = vec![];
    let mut t = Tokenizer::new(file, input);

    while !t.is_done() {
        // parse whitespace separately
        while !t.is_done() {
            match t.tail() {
                _ if t.matches(r" ")  => t.skip_ws(1),
                _ if t.matches(r"\n") => t.skip_ws(1),
                _ if t.matches(r"//") => {
                    while !t.is_done() && !t.matches(r"\n") {
                        t.skip_ws(1);
                    }
                }
                _ => break,
            }
        }

        // now parse the actual token
        tokens.push(match t.tail() {
            // symbols
            _ if t.matches(r"[a-zA-Z_][a-zA-Z_0-9]*") => {
                t.munch(Tt::Sym)
            },
            _ if t.matches(r"[0-9][xX]?[0-9a-fA-F]*") => {
                t.munch(Tt::Number)
            },
            _ if t.matches(r#"['"][^'"]*['"]"#) => {
                t.munch(Tt::String)
            }
            // tokens
            _ if t.matches(r"\(") => t.munch(Tt::LParen),
            _ if t.matches(r"\)") => t.munch(Tt::RParen),
            _ if t.matches(r"\{") => t.munch(Tt::LSquiggle),
            _ if t.matches(r"\}") => t.munch(Tt::RSquiggle),
            _ if t.matches(r"\[") => t.munch(Tt::LSquare),
            _ if t.matches(r"\]") => t.munch(Tt::RSquare),
            _ if t.matches(r"=>") => t.munch(Tt::BigArrow),
            _ if t.matches(r"==") => t.munch(Tt::Eq),
            _ if t.matches(r"=")  => t.munch(Tt::Assign),
            _ if t.matches(r"\.") => t.munch(Tt::Dot),
            _ if t.matches(r"&")  => t.munch(Tt::And),
            _ if t.matches(r"\*") => t.munch(Tt::Splat),
            _ if t.matches(r"->") => t.munch(Tt::Arrow),
            _ if t.matches(r"-")  => t.munch(Tt::Sub),
            _ if t.matches(r",")  => t.munch(Tt::Comma),
            _ if t.matches(r";")  => t.munch(Tt::Semi),
            // wait, end of input?
            "" => t.munch(Tt::TrailingWs),
            // unknown token
            _ => return Err(t.unknown()),
        });
    }

    Ok(tokens)
}
