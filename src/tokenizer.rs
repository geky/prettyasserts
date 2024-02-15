use regex::Regex;

use std::collections::HashMap;
use std::cmp::min;
use std::path::Path;
use std::rc::Rc;
use std::borrow::Cow;

use crate::errors::ParseError;


// lexical stuff
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Tt {
    Sym,                // blah1
    Number,             // 0x1234
    String,             // "asdfasdf"
    LParen,             // (
    RParen,             // )
    LSquiggle,          // {
    RSquiggle,          // }
    LSquare,            // [
    RSquare,            // ]
    BigArrow,           // =>
    Arrow,              // ->
    SplatAssign,        // *=
    SlashAssign,        // /=
    ModAssign,          // %=
    ShlAssign,          // <<=
    ShrAssign,          // >>=
    AndAssign,          // &=
    OrAssign,           // |=
    XorAssign,          // ^=
    AddAssign,          // +=
    SubAssign,          // -=
    Shl,                // <<
    Shr,                // >>
    Eq,                 // ==
    Ne,                 // !=
    Le,                 // <=
    Ge,                 // >=
    Lt,                 // <
    Gt,                 // >
    Assign,             // =
    AndAnd,             // &&
    And,                // &
    OrOr,               // ||
    Or,                 // |
    Xor,                // ^
    Tilde,              // ~
    SplatSplatSplat,    // ***
    SplatSplat,         // **
    Splat,              // *
    Slash,              // /
    Mod,                // %
    PlusPlus,           // ++
    Add,                // +
    MinusMinus,         // --
    Sub,                // -
    Not,                // !
    Question,           // ?
    Colon,              // :
    DotDotDot,          // ...
    Dot,                // .
    Comma,              // ,
    Semi,               // ;
    TrailingWs,
}

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub file: &'a Path,
    pub line: usize,
    pub col: usize,
    pub tt: Tt,
    pub lws: &'a str,
    pub tok: &'a str,
}

// this is my big indent no-leak hack
const INDENT: &'static str = concat!(
    "\n",
    //...:....1....:....2....:....3....:....4....:....
    "                                                 ",
    "                                                 "
);


impl<'a> Token<'a> {
    pub fn new(tt: Tt, tok: &'a str) -> Self {
        Self{
            file: Path::new(""),
            line: 1,
            col: 1,
            tt: tt,
            lws: "",
            tok: tok,
        }
    }

    pub fn file_(self, path: &'a Path) -> Self {
        Self{
            file: path,
            ..self
        }
    }

    pub fn line_(self, line: usize) -> Self {
        Self{
            line: line,
            ..self
        }
    }

    pub fn col_(self, col: usize) -> Self {
        Self{
            col: col,
            ..self
        }
    }

    pub fn lws_(self, lws: &'a str) -> Self {
        Self{
            lws: lws,
            ..self
        }
    }

    pub fn indent(self, n: usize) -> Self {
        self.lws_(&INDENT[..n+1])
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
    file: &'a Path,
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
    fn new(
        file: &'a Path,
        line: usize,
        col: usize,
        input: &'a str
    ) -> Self {
        Self{
            file: file,
            line: line,
            col: col,
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

    fn skip_ws(&mut self, n: usize) {
        self._next(n);
        self.found = "";
    }

    fn munch(&mut self, tt: Tt) -> Token<'a> {
        let tok = Token {
            file: self.file,
            line: self.line,
            col: self.col,
            tt: tt,
            lws: &self.input[self.ws_i..self.i],
            tok: self.found,
        };
        self._next(tok.tok.len());
        self.ws_i = self.i;
        self.found = "";
        tok
    }

    fn error(&self, message: String) -> ParseError {
        ParseError::new(self.file.to_path_buf(), self.line, self.col, message)
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
    file: &'a Path,
    input: &'a str
) -> Result<Vec<Token<'a>>, ParseError> {
    tokenize_at(file, 1, 1, input)
}

pub fn tokenize_at<'a>(
    file: &'a Path,
    line: usize,
    col: usize,
    input: &'a str
) -> Result<Vec<Token<'a>>, ParseError> {
    let mut tokens = vec![];
    let mut t = Tokenizer::new(file, line, col, input);

    while !t.is_done() {
        // parse whitespace separately
        while !t.is_done() {
            match t.tail() {
                _ if t.matches(r" ")  => t.skip_ws(1),
                _ if t.matches(r"\n") => t.skip_ws(1),
                _ if t.matches(r"\\") => t.skip_ws(1),
                _ if t.matches(r"//") => {
                    t.skip_ws(2);
                    while !t.is_done() && !t.matches(r"\n") {
                        t.skip_ws(1);
                    }
                }
                _ if t.matches(r"#") => {
                    t.skip_ws(1);
                    while !t.is_done() && !t.matches(r"\n") {
                        if t.matches(r"\\") {
                            t.skip_ws(1);
                            if t.matches(r"\n") {
                                t.skip_ws(1);
                            }
                        } else {
                            t.skip_ws(1);
                        }
                    }
                }
                _ if t.matches(r"/\*") => {
                    t.skip_ws(2);
                    while !t.is_done() && !t.matches(r"\*/") {
                        t.skip_ws(1);
                    }
                    t.skip_ws(2);
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
            _ if t.matches(r"\(")     => t.munch(Tt::LParen),
            _ if t.matches(r"\)")     => t.munch(Tt::RParen),
            _ if t.matches(r"\{")     => t.munch(Tt::LSquiggle),
            _ if t.matches(r"\}")     => t.munch(Tt::RSquiggle),
            _ if t.matches(r"\[")     => t.munch(Tt::LSquare),
            _ if t.matches(r"\]")     => t.munch(Tt::RSquare),
            _ if t.matches(r"=>")     => t.munch(Tt::BigArrow),
            _ if t.matches(r"->")     => t.munch(Tt::Arrow),
            _ if t.matches(r"<<=")    => t.munch(Tt::ShlAssign),
            _ if t.matches(r">>=")    => t.munch(Tt::ShrAssign),
            _ if t.matches(r"&=")     => t.munch(Tt::AndAssign),
            _ if t.matches(r"\|=")    => t.munch(Tt::OrAssign),
            _ if t.matches(r"\^=")    => t.munch(Tt::XorAssign),
            _ if t.matches(r"\*=")    => t.munch(Tt::SplatAssign),
            _ if t.matches(r"/=")     => t.munch(Tt::SlashAssign),
            _ if t.matches(r"%=")     => t.munch(Tt::ModAssign),
            _ if t.matches(r"\+=")    => t.munch(Tt::AddAssign),
            _ if t.matches(r"-=")     => t.munch(Tt::SubAssign),
            _ if t.matches(r"<<")     => t.munch(Tt::Shl),
            _ if t.matches(r">>")     => t.munch(Tt::Shr),
            _ if t.matches(r"==")     => t.munch(Tt::Eq),
            _ if t.matches(r"!=")     => t.munch(Tt::Ne),
            _ if t.matches(r"<=")     => t.munch(Tt::Le),
            _ if t.matches(r">=")     => t.munch(Tt::Ge),
            _ if t.matches(r"<")      => t.munch(Tt::Lt),
            _ if t.matches(r">")      => t.munch(Tt::Gt),
            _ if t.matches(r"=")      => t.munch(Tt::Assign),
            _ if t.matches(r"&&")     => t.munch(Tt::AndAnd),
            _ if t.matches(r"&")      => t.munch(Tt::And),
            _ if t.matches(r"\|\|")   => t.munch(Tt::OrOr),
            _ if t.matches(r"\|")     => t.munch(Tt::Or),
            _ if t.matches(r"\^")     => t.munch(Tt::Xor),
            _ if t.matches(r"~")      => t.munch(Tt::Tilde),
            _ if t.matches(r"\*\*\*") => t.munch(Tt::SplatSplatSplat),
            _ if t.matches(r"\*\*")   => t.munch(Tt::SplatSplat),
            _ if t.matches(r"\*")     => t.munch(Tt::Splat),
            _ if t.matches(r"/")      => t.munch(Tt::Slash),
            _ if t.matches(r"%")      => t.munch(Tt::Mod),
            _ if t.matches(r"\+\+")   => t.munch(Tt::PlusPlus),
            _ if t.matches(r"\+")     => t.munch(Tt::Add),
            _ if t.matches(r"--")     => t.munch(Tt::MinusMinus),
            _ if t.matches(r"-")      => t.munch(Tt::Sub),
            _ if t.matches(r"!")      => t.munch(Tt::Not),
            _ if t.matches(r"\?")     => t.munch(Tt::Question),
            _ if t.matches(r":")      => t.munch(Tt::Colon),
            _ if t.matches(r"\.\.\.") => t.munch(Tt::DotDotDot),
            _ if t.matches(r"\.")     => t.munch(Tt::Dot),
            _ if t.matches(r",")      => t.munch(Tt::Comma),
            _ if t.matches(r";")      => t.munch(Tt::Semi),
            // wait, end of input?
            "" => t.munch(Tt::TrailingWs),
            // unknown token
            _ => return Err(t.unknown()),
        });
    }

    Ok(tokens)
}


// extra utils, just to make tree creation easy
pub fn tok<'a, S: Into<Cow<'a, str>>>(s: S) -> Token<'a> {
    match s.into() {
        Cow::Borrowed(s) => Token::new(Tt::Sym, s),
        // is s is not 'a, best we can do is leak
        Cow::Owned(s)    => Token::new(Tt::Sym, Box::leak(s.into_boxed_str())),
    }
}
