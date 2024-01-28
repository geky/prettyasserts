#![allow(dead_code)]

use std::path::PathBuf;
use std::path::Path;
use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;

// error stuff
#[derive(Debug)]
pub struct ParseError {
    file: PathBuf,
    line: usize,
    col: usize,
    message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{}:{}: error: {}",
            self.file.display(), self.line, self.col, self.message
        )
    }
}

impl std::error::Error for ParseError {}

impl ParseError {
    pub fn new(
        file: &Path,
        line: usize,
        col: usize,
        message: &str,
    ) -> Self {
        Self{file: file.to_path_buf(), line, col, message: message.to_string()}
    }

    pub fn print_context(&self) {
        print!("{}", self);

        // try to also print context, but ignore errors
        if let Ok(f) = File::open(&self.file) {
            let f = BufReader::new(f);
            if let Some(Ok(line)) = f.lines().nth(self.line-1) {
                println!(":\n{}", line);
                for _ in 0..self.col-1 {
                    print!(" ");
                }
                println!("^");
            }
        }

        println!();
    }
}

