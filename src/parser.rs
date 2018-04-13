use std::iter::Peekable;
use std::result::Result as StdResult;
use tokenizer::{Token, Value};

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "expected {}", _0)]
    ExpectedKind(&'static str),
    #[fail(display = "expected token {:?}, found {:?}", _0, _1)]
    ExpectedToken(Token, Token),
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
    Value(Value)
}
#[derive(Debug, PartialEq)]
pub enum AST {
    IHasA(String, Expr)
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
    pub fn statement(&mut self) -> Result<Option<AST>> {
        if let Some(&Token::IHasA) = self.iter.peek() {
            self.iter.next();
            if let Some(Token::Ident(ident)) = self.iter.next() {
                if let Some(&Token::Itz) = self.iter.peek() {
                    self.iter.next();
                    let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;
                    Ok(Some(AST::IHasA(ident, expression)))
                } else {
                    Ok(Some(AST::IHasA(ident, Expr::Value(Value::Noob))))
                }
            } else { return Err(Error::ExpectedKind("ident")); }
        } else { Ok(None) }
    }
    fn parse_two(&mut self) -> Result<(Box<Expr>, Box<Expr>)> {
        let one = self.expression()?.ok_or(Error::UnexpectedEOF)?;
        self.expect(Token::An)?;
        let two = self.expression()?.ok_or(Error::UnexpectedEOF)?;
        Ok((Box::new(one), Box::new(two)))
    }
    pub fn expression(&mut self) -> Result<Option<Expr>> {
        if let Some(&Token::Value(_)) = self.iter.peek() {
            if let Some(Token::Value(val)) = self.iter.next() {
                Ok(Some(Expr::Value(val)))
            } else { unreachable!(); }
        } else if let Some(&Token::SumOf) = self.iter.peek() {
            self.iter.next();
            let (one, two) = self.parse_two()?;
            Ok(Some(Expr::SumOf(one, two)))
        } else if let Some(&Token::DiffOf) = self.iter.peek() {
            self.iter.next();
            let (one, two) = self.parse_two()?;
            Ok(Some(Expr::DiffOf(one, two)))
        } else if let Some(&Token::ProduktOf) = self.iter.peek() {
            self.iter.next();
            let (one, two) = self.parse_two()?;
            Ok(Some(Expr::ProduktOf(one, two)))
        } else if let Some(&Token::QuoshuntOf) = self.iter.peek() {
            self.iter.next();
            let (one, two) = self.parse_two()?;
            Ok(Some(Expr::QuoshuntOf(one, two)))
        } else if let Some(&Token::ModOf) = self.iter.peek() {
            self.iter.next();
            let (one, two) = self.parse_two()?;
            Ok(Some(Expr::ModOf(one, two)))
        } else if let Some(&Token::BiggrOf) = self.iter.peek() {
            self.iter.next();
            let (one, two) = self.parse_two()?;
            Ok(Some(Expr::BiggrOf(one, two)))
        } else if let Some(&Token::SmallrOf) = self.iter.peek() {
            self.iter.next();
            let (one, two) = self.parse_two()?;
            Ok(Some(Expr::SmallrOf(one, two)))
        } else { Ok(None) }
    }
}

pub fn parse<I: IntoIterator<Item = Token>>(input: I) -> Result<Vec<AST>> {
    let mut parser = Parser { iter: input.into_iter().peekable() };
    let mut parsed = Vec::new();
    while parser.iter.peek().is_some() {
        if let Some(ast) = parser.statement()? {
            parsed.push(ast);
        }
    }
    Ok(parsed)
}

#[cfg(test)]
#[test]
fn test() {
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
