#[macro_use] extern crate failure;
#[macro_use] extern crate unic_char_range;
extern crate unic_ucd_name;

pub mod eval;
pub mod parser;
pub mod tokenizer;
pub mod types;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "tokenize error: {}", _0)]
    TokenizeError(tokenizer::Error),
    #[fail(display = "parse error: {}", _0)]
    ParseError(parser::Error),
    #[fail(display = "eval error: {}", _0)]
    EvalError(eval::Error),
}

use eval::EvalParams;
use parser::AST;
use std::io;

/// Convenience function for tokenizing and parsing code
pub fn parse(code: &str) -> Result<Vec<AST>, Error> {
    let tokens = tokenizer::tokenize(code.chars()).map_err(|err| Error::TokenizeError(err))?;
    #[cfg(feature = "debug")] println!("{:#?}", tokens);
    let parsed = parser::parse(tokens).map_err(|err| Error::ParseError(err))?;
    #[cfg(feature = "debug")] println!("{:#?}", parsed);
    Ok(parsed)
}

/// Convenience function for tokenizing, parsing, and evaluating code
pub fn eval<R, W, F>(code: &str, stdin: R, stdout: W, callback: F) -> Result<(), Error>
    where R: io::BufRead,
          W: io::Write,
          F: FnOnce(&mut EvalParams<R, W>)
{
    let parsed = parse(code)?;
    let mut eval = eval::EvalParams::new(stdin, stdout);
    callback(&mut eval);
    eval.scope().eval_all(parsed).map_err(|err| Error::EvalError(err))?;
    Ok(())
}

/// Convenience function for capturing the output of `eval`
pub fn capture<R, F>(code: &str, stdin: R, callback: F) -> Result<String, Error>
    where R: io::BufRead,
          F: FnOnce(&mut EvalParams<R, &mut Vec<u8>>)
{
    let mut output = Vec::new();
    eval(code, stdin, &mut output, callback)?;
    Ok(String::from_utf8(output).expect("Program (somehow) returned non-utf8 data"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use types::Value;

    fn run(code: &str) -> String {
        capture(code, io::empty(), |_| ()).expect("Running test failed")
    }
    #[test]
    fn run_all() {
        assert_eq!(run(include_str!("../tests/fac.lol")), "120\n");
        assert_eq!(run(include_str!("../tests/pow.lol")), "3125\n");
    }

    #[test]
    fn rust_callback() {
        assert_eq!(
            capture(include_str!("../tests/callback.lol"), io::empty(), |eval| {
                eval.bind_func("LOWERIN", Box::new(|values| {
                    Value::Yarn(values[0].clone().cast_yarn().unwrap().to_lowercase())
                }));
            }).expect("Running test failed"),
            "test\n"
        );
    }
}
