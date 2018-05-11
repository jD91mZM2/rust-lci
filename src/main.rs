extern crate lci;

use std::{env, fs, io};

fn main() {
    let file = match env::args().skip(1).next() {
        Some(file) => file,
        None => {
            eprintln!("usage: lci <file>");
            return;
        }
    };

    let input = match fs::read_to_string(file) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("error reading file: {}", err);
            return;
        }
    };

    let stdin = io::stdin();
    let stdin = stdin.lock();
    let stdout = io::stdout();
    let stdout = stdout.lock();

    match lci::eval(&input, stdin, stdout, |_| ()) {
        Ok(()) => (),
        Err(err) => {
            eprintln!("{}", err);
        }
    }
}
