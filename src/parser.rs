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

    BothOf(Box<Expr>, Box<Expr>),
    EitherOf(Box<Expr>, Box<Expr>),
    WonOf(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    AllOf(Vec<Expr>),
    AnyOf(Vec<Expr>),

    BothSaem(Box<Expr>, Box<Expr>),
    Diffrint(Box<Expr>, Box<Expr>),

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
    pub fn line(&mut self) -> Result<Option<AST>> {
        let stmt = self.statement()?;
        match self.iter.next() {
            None | Some(Token::Separator) => Ok(stmt),
            _ => Err(Error::Trailing)
        }
    }
    fn expect(&mut self, token: Token) -> Result<()> {
        match self.iter.next() {
            Some(ref token2) if token == *token2 => Ok(()),
            Some(token2) => Err(Error::ExpectedToken(token, token2)),
            None => Err(Error::UnexpectedEOF)
        }
    }
    fn statement(&mut self) -> Result<Option<AST>> {
        match self.iter.peek() {
            Some(&Token::IHasA) => {
                self.iter.next();
                if let Some(Token::Ident(ident)) = self.iter.next() {
                    match self.iter.peek() {
                        Some(&Token::Itz) => {
                            self.iter.next();
                            let expression = self.expect_expr()?;
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
                            let expression = self.expect_expr()?;
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
                        inner_.push(AST::It(self.expect_expr()?));
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
        let one = self.expect_expr()?;
        self.expect(Token::An)?;
        let two = self.expect_expr()?;
        Ok((Box::new(one), Box::new(two)))
    }
    fn expect_expr(&mut self) -> Result<Expr> {
        self.expression()?.ok_or(Error::ExpectedKind("expression"))
    }
    fn expression(&mut self) -> Result<Option<Expr>> {
        macro_rules! x_of {
            ($what:path) => {
                {
                    self.iter.next();
                    let (one, two) = self.parse_two()?;
                    Ok(Some($what(one, two)))
                }
            }
        }
        match self.iter.peek() {
            Some(&Token::Value(_)) => {
                if let Some(Token::Value(val)) = self.iter.next() {
                    Ok(Some(Expr::Value(val)))
                } else { unreachable!(); }
            },
            Some(&Token::SumOf) => x_of!(Expr::SumOf),
            Some(&Token::DiffOf) => x_of!(Expr::DiffOf),
            Some(&Token::ProduktOf) => x_of!(Expr::ProduktOf),
            Some(&Token::QuoshuntOf) => x_of!(Expr::QuoshuntOf),
            Some(&Token::ModOf) => x_of!(Expr::ModOf),
            Some(&Token::BiggrOf) => x_of!(Expr::BiggrOf),
            Some(&Token::SmallrOf) => x_of!(Expr::SmallrOf),

            Some(&Token::BothOf) => x_of!(Expr::BothOf),
            Some(&Token::EitherOf) => x_of!(Expr::EitherOf),
            Some(&Token::WonOf) => x_of!(Expr::WonOf),
            Some(&Token::AllOf) => {
                self.iter.next();
                let mut all = Vec::new();
                all.push(self.expect_expr()?);
                loop {
                    match self.iter.peek() {
                        Some(&Token::An) => {
                            self.iter.next();
                            all.push(self.expect_expr()?);
                        },
                        Some(&Token::Mkay) => { self.iter.next(); break },
                        None | Some(&Token::Separator) => break,
                        _ => return Err(Error::Trailing)
                    }
                }
                Ok(Some(Expr::AllOf(all)))
            },
            Some(&Token::AnyOf) => Ok(None),

            Some(&Token::BothSaem) => x_of!(Expr::BothSaem),
            Some(&Token::Diffrint) => x_of!(Expr::Diffrint),
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
            &[AST::IHasA("VAR".to_string(), Expr::ProduktOf(
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
            &[AST::R("VAR".to_string(), Expr::Value(Value::Numbr(12)))]
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
            &[
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
    #[test]
    fn boolean() {
        assert_eq!(
            parse(vec![
                Token::AllOf,
                    Token::BothSaem, Token::Value(Value::Numbr(1)), Token::An, Token::Value(Value::Numbr(1)),
                    Token::An,
                    Token::Diffrint, Token::Value(Value::Numbr(2)), Token::An, Token::Value(Value::Numbr(3))
            ]).unwrap(),
            &[AST::It(Expr::AllOf(vec![
                Expr::BothSaem(Box::new(Expr::Value(Value::Numbr(1))), Box::new(Expr::Value(Value::Numbr(1)))),
                Expr::Diffrint(Box::new(Expr::Value(Value::Numbr(2))), Box::new(Expr::Value(Value::Numbr(3))))
            ]))]
        )
    }
}
