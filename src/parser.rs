#![allow(dead_code)]

use std::path::Path;

use crate::tokenizer::tokenize;
use crate::errors::ParseError;


// tree stuff
pub struct Tree();


// entry point
pub fn parse(path: &Path, input: &str) -> Result<Tree, ParseError> {
    // first tokenize
    let tokens = tokenize(path, input)?;

    // debugging...
    println!("{:#?}", tokens);

    todo!();
}
