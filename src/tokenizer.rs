#![allow(dead_code)]

use regex::Regex;

use std::collections::HashMap;
use std::cmp::min;
use std::path::Path;

use crate::errors::ParseError;


// lexical stuff
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Tt {
    Symbol,     // blah1
    Pound,      // #
    TrailingWs,
}

#[derive(Debug)]
pub struct Token<'a> {
    line: usize,
    col: usize,
    tt: Tt,
    ws: &'a str,
    tok: &'a str,
}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.tt == other.tt
    }
}

impl Eq for Token<'_> {}

impl<'a> Token<'a> {
    fn new(line: usize, col: usize, tt: Tt, ws: &'a str, tok: &'a str) -> Self {
        Self{line, col, tt, ws, tok}
    }
}

// helper for pattern matching
struct Matcher<'a> {
    // cache for regex
    cache: HashMap<&'static str, Regex>,
    // last match
    found: Option<&'a str>,
}

impl<'a> Matcher<'a> {
    fn new() -> Self {
        Self{cache: HashMap::new(), found: None}
    }

    fn match_(&mut self, s: &'a str, p: &'static str) -> Option<&'a str> {
        let r = self.cache.entry(p)
            .or_insert_with(|| {
                // we need to inject a ^ because of course we do, at least
                // this gets cached
                let p = format!("^{}", p);
                Regex::new(&p).unwrap()
            });
        let found = r.find(s).map(|m| m.as_str());
        // save the last match
        self.found = found;
        found
    }

    fn matches(&mut self, s: &'a str, p: &'static str) -> bool {
        self.match_(s, p).is_some()
    }
}

// tokenizer
pub fn tokenize<'a>(
    file: &Path,
    input: &'a str
) -> Result<Vec<Token<'a>>, ParseError> {
    let mut tokens = vec![];
    let mut m = Matcher::new();
    let mut line = 1usize;
    let mut col = 1usize;
    let mut i = 0usize;

    while i < input.len() {
        // parse whitespace separately
        let ws_i = i;
        while i < input.len() {
            match &input[i..] {
                s if m.matches(s, r" ") => {
                    i += 1;
                    col += 1;
                }
                s if m.matches(s, r"\n") => {
                    i += 1;
                    col = 1;
                    line += 1;
                }
                _ => break,
            }
        }
        let ws = &input[ws_i..i];

        // now parse the actual token
        let (tt, tok) = match &input[i..] {
            // symbols
            s if m.matches(s, r"[a-zA-Z_][a-zA-Z_0-9]*") => {
                (Tt::Symbol, m.found.unwrap())
            },
            // tokens
            s if m.matches(s, r"#") => (Tt::Pound, m.found.unwrap()),
            // wait, end of input?
            "" => (Tt::TrailingWs, ""),
            // unknown token
            s => {
                return Err(ParseError::new(file, line, col, &format!(
                    "Unknown token: \"{}...\"",
                    &s[..min(s.len(), 8)]
                )));
            }
        };

        tokens.push(Token::new(line, col, tt, ws, tok));
        i += tok.len();
        col += tok.len();
    }

    Ok(tokens)
}
