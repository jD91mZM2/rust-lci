#[macro_use] extern crate failure;
#[macro_use] extern crate unic_char_range;
extern crate unic_ucd_name;

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
    let scope = eval::Scope::new(stdin, stdout);
    scope.eval_all(parsed).map_err(|err| Error::EvalError(err))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::BufReader;

    fn run(code: &str) -> String {
        let mut output = Vec::new();
        eval(code, BufReader::new(io::stdin()), &mut output).expect("Running test failed");
        String::from_utf8(output).expect("Test returned non-utf8 data")
    }
    #[test]
    fn run_all() {
        assert_eq!(run(include_str!("../tests/fac.lol")), "120\n");
    }
}
