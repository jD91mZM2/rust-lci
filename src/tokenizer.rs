use std::{
    borrow::Cow,
    char as stdchar,
    iter::Peekable,
    result::Result as StdResult
};
use unic_ucd_name::Name as UnicName;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "invalid character in identifier: {}", _0)]
    InvalidIdent(char),
    #[fail(display = "invalid number: {:?}", _0)]
    InvalidNumber(String),
    #[fail(display = "invalid unicode character: {}", _0)]
    InvalidUnicode(String),
    #[fail(display = "invalid characters in interpolation: {:?}", _0)]
    InvalidInterpolation(String),
    #[fail(display = "unclosed comment")]
    UnclosedComment,
    #[fail(display = "unclosed interpolation in string")]
    UnclosedInterpolation,
    #[fail(display = "unclosed string")]
    UnclosedString,
    #[fail(display = "unexpected end of file")]
    UnexpectedEOF,
    #[fail(display = "unknown escape character: {}", _0)]
    UnknownEscape(char),
    #[fail(display = "unknown token")]
    UnknownToken,
}

type Result<T> = StdResult<T, Error>;

#[derive(Clone, Debug, PartialEq)]
pub enum Interpolated {
    Str(String),
    Var(String)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Noob,
    Yarn(String),
    YarnRaw(Vec<Interpolated>),
    Numbr(i64),
    Numbar(f64),
    Troof(bool)
}
impl Default for Value {
    fn default() -> Self {
        Value::Noob
    }
}
impl Value {
    pub fn cast_yarn(self) -> Option<String> {
        match self {
            Value::Noob => None,
            Value::Yarn(inner) => Some(inner),
            Value::YarnRaw(_) => panic!("yarn not interpolated yet"),
            Value::Numbr(n) => Some(n.to_string()),
            Value::Numbar(n) => Some(n.to_string()),
            Value::Troof(b) => Some(b.to_string())
        }
    }
    pub fn cast_numbr(&self) -> Option<i64> {
        match *self {
            Value::Noob => None,
            Value::Yarn(ref inner) => Some(inner.parse().unwrap_or(0)),
            Value::YarnRaw(_) => panic!("yarn not interpolated yet"),
            Value::Numbr(n) => Some(n),
            Value::Numbar(n) => Some(n as i64),
            Value::Troof(b) => Some(b as i64)
        }
    }
    pub fn cast_numbar(&self) -> Option<f64> {
        match *self {
            Value::Noob => None,
            Value::Yarn(ref inner) => Some(inner.parse().unwrap_or(0.0)),
            Value::YarnRaw(_) => panic!("yarn not interpolated yet"),
            Value::Numbr(n) => Some(n as f64),
            Value::Numbar(n) => Some(n),
            Value::Troof(b) => Some(b as i64 as f64)
        }
    }
    pub fn is_numbar(&self) -> bool {
        match *self {
            Value::Yarn(ref inner) => inner.parse::<f64>().is_ok(),
            Value::YarnRaw(_) => panic!("yarn not interpolated yet"),
            Value::Numbar(_) => true,
            _ => false
        }
    }
    pub fn cast_troof(&self) -> bool {
        match *self {
            Value::Noob => false,
            Value::Yarn(ref inner) => inner.is_empty(),
            Value::YarnRaw(_) => panic!("yarn not interpolated yet"),
            Value::Numbr(n) => n == 0,
            Value::Numbar(n) => n == 0.0,
            Value::Troof(b) => b
        }
    }
    pub fn interpolate<F>(&mut self, lookup: F) -> Option<String>
        where F: Fn(&str) -> Option<Value>
    {
        let mut string_ = None;
        if let Value::YarnRaw(ref parts) = *self {
            let mut capacity = 0;
            for part in parts {
                if let Interpolated::Str(ref part) = *part {
                    capacity += part.len();
                }
            }
            let mut string = String::with_capacity(capacity);
            for part in parts {
                string.push_str(&match *part {
                    Interpolated::Str(ref part) => Cow::Borrowed(part),
                    Interpolated::Var(ref var) => Cow::Owned(
                        match lookup(var).and_then(Self::cast_yarn) {
                            Some(val) => val,
                            None => return Some(var.clone())
                        }
                    )
                });
            }
            string_ = Some(string);
        }
        if let Some(string) = string_ {
            *self = Value::Yarn(string);
        }
        None
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Value(Value),
    It,

    Separator,

    IHasA,
    Itz,
    R,

    SumOf,
    DiffOf,
    ProduktOf,
    QuoshuntOf,
    ModOf,
    BiggrOf,
    SmallrOf,

    BothOf,
    EitherOf,
    WonOf,
    Not,
    AllOf,
    AnyOf,
    BothSaem,
    Diffrint,

    Smoosh,
    An,
    Mkay,

    ORly,
    YaRly,
    Mebbe,
    NoWai,
    Oic,

    Wtf,
    Omg,
    OmgWtf,
    Gtfo,

    Visible,
    Exclamation,
    Gimmeh
}

#[derive(Clone)]
pub struct Tokenizer<I: Iterator<Item = char> + Clone> {
    iter: Peekable<I>
}

fn is_space(c: char) -> bool {
    c == ' ' || c == '\t'
}

impl<I: Iterator<Item = char> + Clone> Tokenizer<I> {
    fn trim(&mut self) {
        loop {
            match self.iter.peek().cloned() {
                Some(c) if is_space(c) => { self.iter.next(); },
                _ => break
            }
        }
    }
    fn peek(&mut self) -> Option<char> {
        self.trim();
        self.iter.peek().cloned()
    }
    fn word(&mut self) -> String {
        let mut word = String::new();
        loop {
            match self.iter.peek().cloned() {
                Some(c) if is_space(c) => {
                    self.trim();
                    return word;
                },
                None | Some('\n') | Some(',') => return word,
                Some(c) => {
                    self.iter.next();
                    word.push(c);
                }
            }
        }
    }
    pub fn next(&mut self) -> Result<Option<Token>> {
        let c = match self.peek() {
            Some(c) => c,
            None => return Ok(None)
        };
        if c == '"' {
            fn read_until<I: Iterator<Item = char>>(iter: &mut I, c: char) -> Result<String> {
                let mut string = String::new();
                loop {
                    match iter.next() {
                        Some('"') => return Err(Error::UnclosedInterpolation),
                        None => return Err(Error::UnclosedString),

                        Some(c2) if c == c2 => break,
                        Some(c) => string.push(c)
                    }
                }
                Ok(string)
            }
            self.iter.next(); // leading "
            let mut interpolated = Vec::new();
            let mut string = String::new();
            while let Some(c) = self.iter.next() {
                if c == ':' {
                    match self.iter.next() {
                        Some(')') => string.push('\n'),
                        Some('>') => string.push('\t'),
                        Some('o') => string.push('\x07'),
                        Some('"') => string.push('"'),
                        Some(':') => string.push(':'),
                        Some('(') => {
                            let mut hex = read_until(&mut self.iter, ')')?;
                            let num = match u32::from_str_radix(&hex, 16) {
                                Ok(num) => num,
                                Err(_) => return Err(Error::InvalidNumber(hex))
                            };
                            match stdchar::from_u32(num) {
                                Some(c) => string.push(c),
                                None => return Err(Error::InvalidUnicode(hex))
                            }
                        },
                        Some('{') => {
                            let mut var = read_until(&mut self.iter, '}')?;
                            match var.chars().next() {
                                None |
                                Some('0'...'9') => return Err(Error::InvalidInterpolation(var)),
                                _ => ()
                            }
                            if !var.chars().all(|c|
                                    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                                    (c >= '0' && c <= '9') || c == '_') {
                                return Err(Error::InvalidInterpolation(var));
                            }
                            if !string.is_empty() {
                                interpolated.push(Interpolated::Str(string));
                            }
                            interpolated.push(Interpolated::Var(var));
                            string = String::new();
                        },
                        Some('[') => {
                            let mut name = read_until(&mut self.iter, ']')?.to_uppercase();
                            let mut unicode = None;
                            for c in chars!(..) {
                                if UnicName::of(c)
                                        .map(|n| n.to_string().to_uppercase() == name)
                                        .unwrap_or(false) {
                                    unicode = Some(c);
                                    break;
                                }
                            }
                            match unicode {
                                Some(c) => string.push(c),
                                None => return Err(Error::InvalidUnicode(name))
                            }
                        },
                        Some(c) => return Err(Error::UnknownEscape(c)),
                        None => return Err(Error::UnclosedString)
                    };
                    continue;
                } else if c == '"' {
                    break;
                }
                string.push(c);
            }
            if interpolated.is_empty() {
                return Ok(Some(Token::Value(Value::Yarn(string))));
            } else {
                if !string.is_empty() {
                    interpolated.push(Interpolated::Str(string));
                }
                return Ok(Some(Token::Value(Value::YarnRaw(interpolated))));
            }
        } else if c == '\n' || c == ',' {
            self.iter.next();
            return Ok(Some(Token::Separator));
        }

        let word = self.word();
        match &*word {
            "BTW" => {
                loop {
                    match self.iter.next() {
                        Some('\n') | None => break,
                        _ => ()
                    }
                }
                return self.next();
            },
            "OBTW" => {
                loop {
                    match self.peek() {
                        None => return Err(Error::UnclosedComment),
                        Some('T') => {
                            if self.word() == "TLDR" {
                                return self.next();
                            } else {
                                self.iter.next();
                            }
                        },
                        _ => { self.iter.next(); },
                    }
                }
            },
            "IT" => return Ok(Some(Token::It)),
            "I" => {
                let mut clone = self.clone();
                if clone.word() == "HAS" {
                    if clone.word() == "A" {
                        *self = clone;
                        return Ok(Some(Token::IHasA));
                    }
                }
            },
            "ITZ" => return Ok(Some(Token::Itz)),
            "R" => return Ok(Some(Token::R)),
            "SUM" | "DIFF" | "PRODUKT" | "QUOSHUNT" | "MOD" | "BIGGR" | "SMALLR" |
            "BOTH" | "EITHER" | "WON" | "ALL" | "ANY" => {
                let mut clone = self.clone();
                match &*clone.word() {
                    "OF" => {
                        *self = clone;
                        return Ok(Some(match &*word {
                            "SUM" => Token::SumOf,
                            "DIFF" => Token::DiffOf,
                            "PRODUKT" => Token::ProduktOf,
                            "QUOSHUNT" => Token::QuoshuntOf,
                            "MOD" => Token::ModOf,
                            "BIGGR" => Token::BiggrOf,
                            "SMALLR" => Token::SmallrOf,

                            "BOTH" => Token::BothOf,
                            "EITHER" => Token::EitherOf,
                            "WON" => Token::WonOf,
                            "ALL" => Token::AllOf,
                            "ANY" => Token::AnyOf,

                            _ => unreachable!()
                        }));
                    },
                    "SAEM" if word == "BOTH" => {
                        *self = clone;
                        return Ok(Some(Token::BothSaem));
                    },
                    _ => ()
                }
            },
            "NOT" => return Ok(Some(Token::Not)),
            "DIFFRINT" => return Ok(Some(Token::Diffrint)),
            "SMOOSH" => return Ok(Some(Token::Smoosh)),
            "AN" => return Ok(Some(Token::An)),
            "MKAY" => return Ok(Some(Token::Mkay)),
            "O" => {
                let mut clone = self.clone();
                if clone.word() == "RLY?" {
                    *self = clone;
                    return Ok(Some(Token::ORly));
                }
            },
            "YA" => {
                let mut clone = self.clone();
                if clone.word() == "RLY" {
                    *self = clone;
                    return Ok(Some(Token::YaRly));
                }
            },
            "MEBBE" => return Ok(Some(Token::Mebbe)),
            "NO" => {
                let mut clone = self.clone();
                if clone.word() == "WAI" {
                    *self = clone;
                    return Ok(Some(Token::NoWai));
                }
            },
            "OIC" => return Ok(Some(Token::Oic)),
            "WTF?" => return Ok(Some(Token::Wtf)),
            "OMG" => return Ok(Some(Token::Omg)),
            "OMGWTF" => return Ok(Some(Token::OmgWtf)),
            "GTFO" => return Ok(Some(Token::Gtfo)),
            "VISIBLE" => return Ok(Some(Token::Visible)),
            "!" => return Ok(Some(Token::Exclamation)),
            "GIMMEH" => return Ok(Some(Token::Gimmeh)),
            _ => ()
        }

        match c {
            'a'...'z' |
            'A'...'Z' |
            '_' => {
                for c in word.chars() {
                    match c {
                        'a'...'z' |
                        'A'...'Z' |
                        '0'...'9' |
                        '_' => (),
                        c => return Err(Error::InvalidIdent(c))
                    }
                }
                return Ok(Some(Token::Ident(word)));
            },
            '0'...'9' => {
                if let Ok(num) = word.parse::<i64>() {
                    return Ok(Some(Token::Value(Value::Numbr(num))));
                } else if let Ok(num) = word.parse::<f64>() {
                    return Ok(Some(Token::Value(Value::Numbar(num))));
                }
                return Err(Error::InvalidNumber(word));
            },
            _ => ()
        }

        Err(Error::UnknownToken)
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>> {
    let mut tokenizer = Tokenizer { iter: input.chars().peekable() };
    let mut tokens = Vec::new();
    while let Some(token) = tokenizer.next()? {
        tokens.push(token);
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn strings() {
        assert_eq!(
            tokenize(r#" "Hello World :) How are you :>? I'm:: :"fine:"" "#).unwrap(),
            &[Token::Value(Value::Yarn("Hello World \n How are you \t? I'm: \"fine\"".to_string()))]
        );
    }
    #[test]
    fn assign() {
        assert_eq!(
            tokenize("I HAS A VAR ITZ 12           BTW this is a comment").unwrap(),
            &[Token::IHasA, Token::Ident("VAR".to_string()), Token::Itz, Token::Value(Value::Numbr(12))]
        );
    }
    #[test]
    fn sum_of() {
        assert_eq!(
            tokenize("SUM OF OBTW hi TLDR 2 AN 4").unwrap(),
            &[Token::SumOf, Token::Value(Value::Numbr(2)), Token::An, Token::Value(Value::Numbr(4))]
        );
    }
    #[test]
    fn ifs() {
        assert_eq!(
            tokenize("\
                BOTH SAEM 1 AN 1, O RLY?
                    YA RLY, RESULT R \"YES\"
                    MEBBE BOTH SAEM 1 AN 2, RESULT R \"CLOSE\"
                    NO WAI, RESULT R \"NO\"
                OIC\
            ").unwrap(),
            &[
                Token::BothSaem, Token::Value(Value::Numbr(1)), Token::An, Token::Value(Value::Numbr(1)), Token::Separator,
                Token::ORly, Token::Separator,
                    Token::YaRly, Token::Separator,
                        Token::Ident("RESULT".to_string()), Token::R, Token::Value(Value::Yarn("YES".to_string())),
                        Token::Separator,
                    Token::Mebbe,
                        Token::BothSaem, Token::Value(Value::Numbr(1)), Token::An, Token::Value(Value::Numbr(2)),
                        Token::Separator,
                        Token::Ident("RESULT".to_string()), Token::R, Token::Value(Value::Yarn("CLOSE".to_string())),
                        Token::Separator,
                    Token::NoWai, Token::Separator,
                        Token::Ident("RESULT".to_string()), Token::R, Token::Value(Value::Yarn("NO".to_string())),
                        Token::Separator,
                Token::Oic
            ]
        )
    }
}
