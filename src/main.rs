extern crate lci;

use std::{
    env,
    fs::File,
    io::{self, Read}
};

fn main() {
    let file = match env::args().skip(1).next() {
        Some(file) => file,
        None => {
            eprintln!("usage: lci <file>");
            return;
        }
    };

    let mut input = String::new();
    match File::open(file).and_then(|mut file| file.read_to_string(&mut input)) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("error reading file: {}", err);
            return;
        }
    }

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
