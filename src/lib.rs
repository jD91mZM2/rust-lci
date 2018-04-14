#[macro_use] extern crate failure;

pub mod eval;
pub mod tokenizer;
pub mod parser;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "tokenize error: {}", _0)]
    TokenizeError(tokenizer::Error),
    #[fail(display = "parse error: {}", _0)]
    ParseError(parser::Error),
    #[fail(display = "eval error: {}", _0)]
    EvalError(eval::Error),
}

use std::io;

pub fn eval<R: io::BufRead, W: io::Write>(input: &str, stdin: R, stdout: W) -> Result<(), Error> {
    let tokens = tokenizer::tokenize(input).map_err(|err| Error::TokenizeError(err))?;
    let parsed = parser::parse(tokens).map_err(|err| Error::ParseError(err))?;
    let mut scope = eval::Scope::new(stdin, stdout);
    scope.eval_all(parsed).map_err(|err| Error::EvalError(err))?;
    Ok(())
}
