use std::{
    iter::Peekable,
    result::Result as StdResult
};
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
    Var(String),
    Value(Value),
    It,

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

    Smoosh(Vec<Expr>)
}
#[derive(Debug, PartialEq)]
pub enum AST {
    IHasA(String, Expr),
    R(String, Expr),
    It(Expr),
    ORly(Vec<AST>, Vec<(Expr, Vec<AST>)>, Vec<AST>),
    Wtf(Vec<(Expr, Vec<AST>)>, Vec<AST>),

    Gtfo,

    Visible(Vec<Expr>, bool),
    Gimmeh(String),
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
    fn block(&mut self, until: &[Token]) -> Result<Vec<AST>> {
        let mut block = Vec::new();
        loop {
            match self.iter.peek() {
                Some(token) => if until.contains(&token) { break; },
                None => ()
            }
            if let Some(line) = self.line()? {
                println!("> {:?}", line);
                block.push(line);
            }
        }
        Ok(block)
    }
    fn expect(&mut self, token: Token) -> Result<()> {
        match self.iter.next() {
            Some(ref token2) if token == *token2 => Ok(()),
            Some(token2) => Err(Error::ExpectedToken(token, token2)),
            None => Err(Error::UnexpectedEOF)
        }
    }
    fn expect_peek(&mut self, token: Token) -> Result<()> {
        match self.iter.peek() {
            Some(token2) if token == *token2 => Ok(()),
            Some(token2) => Err(Error::ExpectedToken(token, token2.clone())),
            None => Err(Error::UnexpectedEOF)
        }
    }
    fn trim(&mut self) {
        while let Some(&Token::Separator) = self.iter.peek() {
            self.iter.next();
        }
    }
    fn statement(&mut self) -> Result<Option<AST>> {
        match self.iter.peek() {
            Some(&Token::Gtfo) => {
                self.iter.next();
                Ok(Some(AST::Gtfo))
            },
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
                } else { Err(Error::ExpectedKind("ident")) }
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
                self.trim();
                self.expect(Token::YaRly)?;
                self.expect(Token::Separator)?;
                let mut yarly = self.block(&[Token::Mebbe, Token::NoWai, Token::Oic])?;

                let mut mebbe = Vec::new();
                self.trim();
                while let Some(&Token::Mebbe) = self.iter.peek() {
                    self.iter.next();
                    let condition = self.expect_expr()?;
                    self.expect(Token::Separator)?;
                    let mut block = self.block(&[Token::Mebbe, Token::NoWai, Token::Oic])?;
                    self.trim();

                    mebbe.push((condition, block));
                }

                let nowai = if let Some(&Token::NoWai) = self.iter.peek() {
                    self.iter.next();
                    self.expect(Token::Separator)?;
                    self.block(&[Token::Oic])?
                } else { Vec::new() };
                self.expect(Token::Oic)?;
                Ok(Some(AST::ORly(yarly, mebbe, nowai)))
            },
            Some(&Token::Wtf) => {
                self.iter.next();
                self.expect(Token::Separator)?;
                self.trim();
                self.expect_peek(Token::Omg)?;

                let mut omg = Vec::new();
                while let Some(&Token::Omg) = self.iter.peek() {
                    self.iter.next();
                    let expr = self.expect_expr()?;
                    self.expect(Token::Separator)?;
                    let mut block = self.block(&[Token::Omg, Token::OmgWtf, Token::Oic])?;
                    self.trim();

                    omg.push((expr, block));
                }
                let mut omgwtf = if let Some(&Token::OmgWtf) = self.iter.peek() {
                    self.iter.next();
                    self.expect(Token::Separator)?;
                    self.block(&[Token::Oic])?
                } else { Vec::new() };
                self.expect(Token::Oic)?;
                Ok(Some(AST::Wtf(omg, omgwtf)))
            },
            Some(&Token::Visible) => {
                self.iter.next();
                let mut exprs = Vec::new();
                let newline = loop {
                    exprs.push(self.expect_expr()?);
                    match self.iter.peek() {
                        Some(&Token::Exclamation) => { self.iter.next(); break false },
                        None | Some(&Token::Separator) => break true,
                        _ => ()
                    }
                };
                Ok(Some(AST::Visible(exprs, newline)))
            },
            Some(&Token::Gimmeh) => {
                self.iter.next();
                if let Some(Token::Ident(ident)) = self.iter.next() {
                    Ok(Some(AST::Gimmeh(ident)))
                } else {
                    Err(Error::ExpectedKind("ident"))
                }
            },
            _ => Ok(self.expression()?.map(|expr| AST::It(expr)))
        }
    }
    fn two_exprs(&mut self) -> Result<(Box<Expr>, Box<Expr>)> {
        let one = self.expect_expr()?;
        if let Some(&Token::An) = self.iter.peek() {
            self.iter.next();
        }
        let two = self.expect_expr()?;
        Ok((Box::new(one), Box::new(two)))
    }
    fn multiple_exprs(&mut self) -> Result<Vec<Expr>> {
        let mut all = Vec::new();
        all.push(self.expect_expr()?);
        loop {
            match self.iter.peek() {
                Some(&Token::Mkay) => { self.iter.next(); break },
                None | Some(&Token::Separator) => break,
                Some(&Token::An) => {
                    self.iter.next();
                    all.push(self.expect_expr()?);
                },
                _ => all.push(self.expect_expr()?)
            }
        }
        Ok(all)
    }
    fn expect_expr(&mut self) -> Result<Expr> {
        self.expression()?.ok_or(Error::ExpectedKind("expression"))
    }
    fn expression(&mut self) -> Result<Option<Expr>> {
        macro_rules! x_of {
            ($what:path) => {
                {
                    self.iter.next();
                    let (one, two) = self.two_exprs()?;
                    Ok(Some($what(one, two)))
                }
            }
        }
        match self.iter.peek() {
            Some(&Token::It) => {
                self.iter.next();
                Ok(Some(Expr::It))
            }
            Some(&Token::Value(_)) => {
                if let Some(Token::Value(val)) = self.iter.next() {
                    Ok(Some(Expr::Value(val)))
                } else { unreachable!(); }
            },
            Some(&Token::Ident(_)) => {
                if let Some(Token::Ident(var)) = self.iter.next() {
                    Ok(Some(Expr::Var(var)))
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
            Some(&Token::Not) => {
                self.iter.next();
                let expr = self.expect_expr()?;
                Ok(Some(Expr::Not(Box::new(expr))))
            },
            Some(&Token::AllOf) => {
                self.iter.next();
                Ok(Some(Expr::AllOf(self.multiple_exprs()?)))
            },
            Some(&Token::AnyOf) => {
                self.iter.next();
                Ok(Some(Expr::AnyOf(self.multiple_exprs()?)))
            },

            Some(&Token::BothSaem) => x_of!(Expr::BothSaem),
            Some(&Token::Diffrint) => x_of!(Expr::Diffrint),

            Some(&Token::Smoosh) => {
                self.iter.next();
                Ok(Some(Expr::Smoosh(self.multiple_exprs()?)))
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
            println!("{:?}", ast);
            parsed.push(ast);
        }
    }
    Ok(parsed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assign() {
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
    fn troofs() {
        assert_eq!(
            parse(vec![
                Token::AllOf,
                    Token::BothSaem, Token::Value(Value::Numbr(1)), Token::An, Token::Value(Value::Numbr(1)),
                    Token::An,
                    Token::Not, Token::Diffrint, Token::Value(Value::Numbr(2)), Token::An, Token::Value(Value::Numbr(2))
            ]).unwrap(),
            &[AST::It(Expr::AllOf(vec![
                Expr::BothSaem(Box::new(Expr::Value(Value::Numbr(1))), Box::new(Expr::Value(Value::Numbr(1)))),
                Expr::Not(Box::new(
                    Expr::Diffrint(Box::new(Expr::Value(Value::Numbr(2))), Box::new(Expr::Value(Value::Numbr(2))))
                ))
            ]))]
        )
    }
    #[test]
    fn nested_orlys() {
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
                    vec![(Expr::Value(Value::Troof(false)),
                          vec![AST::It(Expr::Value(Value::Numbr(3)))])],
                    vec![AST::It(Expr::Value(Value::Troof(true))),
                         AST::ORly(
                             vec![AST::It(Expr::Value(Value::Numbr(7)))],
                             Vec::new(),
                             Vec::new()
                         )]
                )
            ]
        );
    }
    #[test]
    fn wtf() {
        assert_eq!(
            parse(vec![
                Token::SumOf, Token::Value(Value::Numbr(1)), Token::An, Token::Value(Value::Numbr(3)), Token::Separator,
                Token::Wtf, Token::Separator,
                Token::Omg, Token::Value(Value::Numbr(1)), Token::Separator,
                    Token::Visible, Token::Value(Value::Yarn("WHAT, NO".to_string())), Token::Separator,
                Token::Omg, Token::Value(Value::Numbr(2)), Token::Separator,
                Token::Omg, Token::Value(Value::Numbr(3)), Token::Separator,
                    Token::Visible, Token::Value(Value::Yarn("R U STUPID?".to_string())), Token::Separator,
                    Token::Gtfo, Token::Separator,
                Token::Omg, Token::Value(Value::Numbr(4)), Token::Separator,
                    Token::Visible, Token::Value(Value::Yarn("CORREC!".to_string())), Token::Separator,
                    Token::Gtfo, Token::Separator,
                Token::OmgWtf, Token::Separator,
                    Token::Visible, Token::Value(Value::Yarn("IDFK".to_string())), Token::Separator,
                    Token::Gtfo, Token::Separator,
                Token::Oic
            ]).unwrap(),
            &[
                AST::It(Expr::SumOf(Box::new(Expr::Value(Value::Numbr(1))), Box::new(Expr::Value(Value::Numbr(3))))),
                AST::Wtf(
                    vec![(Expr::Value(Value::Numbr(1)),
                        vec![AST::Visible(vec![Expr::Value(Value::Yarn("WHAT, NO".to_string()))], true)]),
                         (Expr::Value(Value::Numbr(2)), vec![]),
                         (Expr::Value(Value::Numbr(3)),
                        vec![AST::Visible(vec![Expr::Value(Value::Yarn("R U STUPID?".to_string()))], true), AST::Gtfo]),
                         (Expr::Value(Value::Numbr(4)),
                        vec![AST::Visible(vec![Expr::Value(Value::Yarn("CORREC!".to_string()))], true), AST::Gtfo])],
                     vec![AST::Visible(vec![Expr::Value(Value::Yarn("IDFK".to_string()))], true),
                          AST::Gtfo]
                )
            ]
        );
    }
}
