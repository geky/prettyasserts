#![allow(dead_code)]

use std::path::PathBuf;
use std::rc::Rc;

use crate::tokenizer::Token;
use crate::errors::ParseError;


// tree stuff
#[derive(Debug, Clone)]
pub struct Tree();


// entry point
pub fn parse(
    path: &Rc<PathBuf>,
    tokens: &[Token<'_>]
) -> Result<Tree, ParseError> {
    // debugging...
    println!("{:#?}", tokens);

    todo!();
}
