use std::iter::Peekable;
use std::result::Result as StdResult;
use tokenizer::{Token, Value};

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "expected {}", _0)]
    ExpectedKind(&'static str),
    #[fail(display = "expected token {:?}, found {:?}", _0, _1)]
    ExpectedToken(Token, Token),
    #[fail(display = "trailing characters after statement")]
    Trailing,
    #[fail(display = "unexpected end of file")]
    UnexpectedEOF
}

type Result<T> = StdResult<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Expr {
    SumOf(Box<Expr>, Box<Expr>),
    DiffOf(Box<Expr>, Box<Expr>),
    ProduktOf(Box<Expr>, Box<Expr>),
    QuoshuntOf(Box<Expr>, Box<Expr>),
    ModOf(Box<Expr>, Box<Expr>),
    BiggrOf(Box<Expr>, Box<Expr>),
    SmallrOf(Box<Expr>, Box<Expr>),
    Var(String),
    Value(Value)
}
#[derive(Debug, PartialEq)]
pub enum AST {
    IHasA(String, Expr),
    R(String, Expr),
    It(Expr),
    ORly(Vec<AST>, Vec<AST>)
}

pub struct Parser<I: Iterator<Item = Token>> {
    iter: Peekable<I>
}
impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn expect(&mut self, token: Token) -> Result<()> {
        match self.iter.next() {
            Some(ref token2) if token == *token2 => Ok(()),
            Some(token2) => Err(Error::ExpectedToken(token, token2)),
            None => Err(Error::UnexpectedEOF)
        }
    }
    pub fn line(&mut self) -> Result<Option<AST>> {
        let stmt = self.statement()?;
        match self.iter.next() {
            None | Some(Token::Separator) => Ok(stmt),
            _ => Err(Error::Trailing)
        }
    }
    pub fn statement(&mut self) -> Result<Option<AST>> {
        match self.iter.peek() {
            Some(&Token::IHasA) => {
                self.iter.next();
                if let Some(Token::Ident(ident)) = self.iter.next() {
                    match self.iter.peek() {
                        Some(&Token::Itz) => {
                            self.iter.next();
                            let expression = self.expression()?.ok_or(Error::ExpectedKind("token"))?;
                            Ok(Some(AST::IHasA(ident, expression)))
                        },
                        None | Some(&Token::Separator) => {
                            Ok(Some(AST::IHasA(ident, Expr::Value(Value::Noob))))
                        },
                        _ => Err(Error::Trailing)
                    }
                } else { return Err(Error::ExpectedKind("ident")); }
            },
            Some(&Token::Ident(_)) => {
                if let Some(Token::Ident(ident)) = self.iter.next() {
                    match self.iter.peek() {
                        Some(&Token::R) => {
                            self.iter.next();
                            let expression = self.expression()?.ok_or(Error::ExpectedKind("token"))?;
                            Ok(Some(AST::R(ident, expression)))
                        },
                        None | Some(&Token::Separator) => {
                            Ok(Some(AST::It(Expr::Var(ident))))
                        },
                        _ => Err(Error::Trailing)
                    }
                } else { unreachable!(); }
            },
            Some(&Token::ORly) => {
                self.iter.next();
                self.expect(Token::Separator)?;
                self.expect(Token::YaRly)?;
                self.expect(Token::Separator)?;
                let mut yarly = Vec::new();
                loop {
                    match self.iter.peek() {
                        Some(&Token::Mebbe) |
                        Some(&Token::NoWai) |
                        Some(&Token::Oic) => break,
                        _ => if let Some(line) = self.line()? {
                            yarly.push(line);
                        }
                    }
                }
                let mut nowai = Vec::new();
                {
                    let mut inner: *mut Vec<AST> = &mut nowai;
                    // pointer because rust won't let us keep a reference alive for long enough
                    while let Some(&Token::Mebbe) = self.iter.peek() {
                        self.iter.next();
                        let inner_ = unsafe { &mut *inner };
                        inner_.push(AST::It(self.expression()?.ok_or(Error::ExpectedKind("expression"))?));
                        let mut mebbe = Vec::new();
                        loop {
                            match self.iter.peek() {
                                Some(&Token::Mebbe) |
                                Some(&Token::NoWai) |
                                Some(&Token::Oic) => break,
                                _ => if let Some(line) = self.line()? {
                                    mebbe.push(line);
                                }
                            }
                        }
                        inner_.push(AST::ORly(mebbe, Vec::new()));
                        if let AST::ORly(_, ref mut ast) = *inner_.last_mut().unwrap() {
                            inner = &mut *ast;
                        }
                    }

                    if let Some(&Token::NoWai) = self.iter.peek() {
                        self.iter.next();
                        let inner_ = unsafe { &mut *inner };
                        loop {
                            match self.iter.peek() {
                                Some(&Token::Oic) => break,
                                _ => if let Some(line) = self.line()? {
                                    inner_.push(line);
                                }
                            }
                        }
                    }
                }
                self.expect(Token::Oic)?;
                Ok(Some(AST::ORly(yarly, nowai)))
            },
            _ => Ok(self.expression()?.map(|expr| AST::It(expr)))
        }
    }
    fn parse_two(&mut self) -> Result<(Box<Expr>, Box<Expr>)> {
        let one = self.expression()?.ok_or(Error::ExpectedKind("token"))?;
        self.expect(Token::An)?;
        let two = self.expression()?.ok_or(Error::ExpectedKind("token"))?;
        Ok((Box::new(one), Box::new(two)))
    }
    pub fn expression(&mut self) -> Result<Option<Expr>> {
        match self.iter.peek() {
            Some(&Token::Value(_)) => {
                if let Some(Token::Value(val)) = self.iter.next() {
                    Ok(Some(Expr::Value(val)))
                } else { unreachable!(); }
            },
            Some(&Token::SumOf) => {
                self.iter.next();
                let (one, two) = self.parse_two()?;
                Ok(Some(Expr::SumOf(one, two)))
            },
            Some(&Token::DiffOf) => {
                self.iter.next();
                let (one, two) = self.parse_two()?;
                Ok(Some(Expr::DiffOf(one, two)))
            },
            Some(&Token::ProduktOf) => {
                self.iter.next();
                let (one, two) = self.parse_two()?;
                Ok(Some(Expr::ProduktOf(one, two)))
            },
            Some(&Token::QuoshuntOf) => {
                self.iter.next();
                let (one, two) = self.parse_two()?;
                Ok(Some(Expr::QuoshuntOf(one, two)))
            },
            Some(&Token::ModOf) => {
                self.iter.next();
                let (one, two) = self.parse_two()?;
                Ok(Some(Expr::ModOf(one, two)))
            },
            Some(&Token::BiggrOf) => {
                self.iter.next();
                let (one, two) = self.parse_two()?;
                Ok(Some(Expr::BiggrOf(one, two)))
            },
            Some(&Token::SmallrOf) => {
                self.iter.next();
                let (one, two) = self.parse_two()?;
                Ok(Some(Expr::SmallrOf(one, two)))
            },
            _ => Ok(None)
        }
    }
}

pub fn parse<I: IntoIterator<Item = Token>>(input: I) -> Result<Vec<AST>> {
    let mut parser = Parser { iter: input.into_iter().peekable() };
    let mut parsed = Vec::new();
    while parser.iter.peek().is_some() {
        if let Some(ast) = parser.line()? {
            parsed.push(ast);
        }
    }
    Ok(parsed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declaration() {
        assert_eq!(
            parse(vec![
                Token::IHasA,
                Token::Ident("VAR".to_string()),
                Token::Itz,
                Token::ProduktOf,
                    Token::SumOf,
                        Token::Value(Value::Numbr(12)), Token::An, Token::Value(Value::Numbar(5.0)),
                    Token::An,
                    Token::Value(Value::Numbr(10))
            ]).unwrap(),
            [AST::IHasA("VAR".to_string(), Expr::ProduktOf(
                Box::new(Expr::SumOf(
                    Box::new(Expr::Value(Value::Numbr(12))),
                    Box::new(Expr::Value(Value::Numbar(5.0)))
                )),
                Box::new(Expr::Value(Value::Numbr(10)))
            ))]
        );
    }
    #[test]
    fn assignment() {
        assert_eq!(
            parse(vec![
                Token::Ident("VAR".to_string()),
                Token::R,
                Token::Value(Value::Numbr(12))
            ]).unwrap(),
            [AST::R("VAR".to_string(), Expr::Value(Value::Numbr(12)))]
        );
    }
    #[test]
    fn nested_ifs() {
        assert_eq!(
            parse(vec![
                Token::Value(Value::Troof(true)), Token::Separator,
                Token::ORly, Token::Separator,
                    Token::YaRly, Token::Separator,
                        Token::Value(Value::Numbr(1)), Token::Separator,
                        Token::Value(Value::Numbr(3)), Token::Separator,
                    Token::Mebbe, Token::Value(Value::Troof(false)), Token::Separator,
                        Token::Value(Value::Numbr(3)), Token::Separator,
                    Token::NoWai, Token::Separator,
                        Token::Value(Value::Troof(true)), Token::Separator,
                        Token::ORly, Token::Separator,
                            Token::YaRly, Token::Separator,
                                Token::Value(Value::Numbr(7)), Token::Separator,
                        Token::Oic, Token::Separator,
                Token::Oic
            ]).unwrap(),
            [
                AST::It(Expr::Value(Value::Troof(true))),
                AST::ORly(
                    vec![AST::It(Expr::Value(Value::Numbr(1))),
                         AST::It(Expr::Value(Value::Numbr(3)))],
                    vec![
                        AST::It(Expr::Value(Value::Troof(false))),
                        AST::ORly(
                            vec![AST::It(Expr::Value(Value::Numbr(3)))],
                            vec![
                                AST::It(Expr::Value(Value::Troof(true))),
                                AST::ORly(
                                    vec![AST::It(Expr::Value(Value::Numbr(7)))],
                                    Vec::new()
                                )
                            ],
                        )
                    ]
                )
            ]
        );
    }
}
