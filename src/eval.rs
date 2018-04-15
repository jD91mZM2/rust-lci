use parser::{AST, Expr, Operation};
use std::{
    cell::RefCell,
    collections::HashMap,
    io,
    result::Result as StdResult
};
use tokenizer::Value;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "cannot cast value to that type")]
    InvalidCast,
    #[fail(display = "loop variable cannot be casted to numbr")]
    InvalidCastLoop,
    #[fail(display = "function {:?} expected {} parameters", _0, _1)]
    InvalidUsage(String, usize),
    #[fail(display = "can't shadow variable from the same scope: {:?}", _0)]
    ShadowVar(String),
    #[fail(display = "undefined function {:?}", _0)]
    UndefinedFunc(String),
    #[fail(display = "undefined variable {:?}", _0)]
    UndefinedVar(String)
}

type Result<T> = StdResult<T, Error>;

pub enum Return {
    None,
    Gtfo,
    Value(Value)
}

struct Function {
    args: Vec<String>,
    block: Vec<AST>
}

pub struct Scope<'a, R: io::BufRead + 'a, W: io::Write + 'a> {
    stdin: Option<RefCell<R>>,
    stdout: Option<RefCell<W>>,

    it: RefCell<Value>,
    vars: RefCell<HashMap<String, Value>>,
    funcs: RefCell<HashMap<String, Function>>,
    parent: Option<&'a Scope<'a, R, W>>
}
impl<'a, R: io::BufRead, W: io::Write> Scope<'a, R, W> {
    pub fn new(stdin: R, stdout: W) -> Self {
        Self {
            stdin: Some(RefCell::new(stdin)),
            stdout: Some(RefCell::new(stdout)),

            it: RefCell::new(Value::Noob),
            vars: RefCell::new(HashMap::new()),
            funcs: RefCell::new(HashMap::new()),
            parent: None
        }
    }

    fn top_parent(&self) -> &Self {
        let mut me = self;
        while let Some(parent) = me.parent {
            me = parent;
        }
        me
    }
    fn find_var<F, T>(&self, name: &str, apply: F) -> Option<T>
        where F: FnOnce(&mut Value) -> T
    {
        let mut me = self;
        loop {
            let mut vars = me.vars.borrow_mut();
            if let Some(var) = vars.get_mut(name) {
                break Some(apply(var));
            } else if let Some(parent) = me.parent {
                me = parent;
            } else {
                break None;
            }
        }
    }
    fn call_func(&self, name: &str, args: Vec<Value>) -> Result<Value> {
        let mut me = self;
        Ok(loop {
            let mut funcs = me.funcs.borrow_mut();
            if let Some(the_func) = funcs.get_mut(name) {
                if args.len() != the_func.args.len() {
                    return Err(Error::InvalidUsage(name.to_string(), the_func.args.len()));
                }

                let mut vars = me.vars.borrow_mut();
                for (i, arg) in args.iter().enumerate() {
                    vars.insert(the_func.args[i].clone(), arg.clone());
                }
                drop(vars);

                break match me.eval_all(the_func.block.clone())? {
                    Return::None => me.it.borrow().clone(),
                    Return::Gtfo => Value::Noob,
                    Return::Value(val) => val
                };
            } else if let Some(parent) = me.parent {
                me = parent;
            } else {
                return Err(Error::UndefinedFunc(name.to_string()));
            }
        })
    }
    fn scope(&'a self) -> Self {
        Self {
            stdin: None,
            stdout: None,

            it: self.it.clone(),
            vars: RefCell::new(HashMap::new()),
            funcs: RefCell::new(HashMap::new()),
            parent: Some(self)
        }
    }

    fn apply_num<F1, F2>(&self, one: Expr, two: Expr, if_numbr: F1, if_numbar: F2) -> Result<Value>
        where F1: FnOnce(i64, i64) -> i64,
              F2: FnOnce(f64, f64) -> f64
    {
        let one = self.eval_expr(one)?;
        let two = self.eval_expr(two)?;
        if one.is_numbar() || two.is_numbar() {
            Ok(Value::Numbar(if_numbar(
                one.cast_numbar().ok_or(Error::InvalidCast)?,
                two.cast_numbar().ok_or(Error::InvalidCast)?
            )))
        } else {
            Ok(Value::Numbr(if_numbr(
                one.cast_numbr().ok_or(Error::InvalidCast)?,
                two.cast_numbr().ok_or(Error::InvalidCast)?
            )))
        }
    }
    fn apply_any<F>(&self, one: Expr, two: Expr, apply: F) -> Result<Value>
        where F: FnOnce(Value, Value) -> bool
    {
        Ok(Value::Troof(apply(self.eval_expr(one)?, self.eval_expr(two)?)))
    }
    fn apply_bool<F>(&self, one: Expr, two: Expr, apply: F) -> Result<Value>
        where F: FnOnce(bool, bool) -> bool
    {
        self.apply_any(one, two, |x, y| apply(x.cast_troof(), y.cast_troof()))
    }
    pub fn eval_expr(&self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::It => Ok(self.it.borrow().clone()),
            Expr::Value(mut val) => {
                if let Some(missing) = val.interpolate(|var| self.find_var(var, |var| var.clone())) {
                    return Err(Error::UndefinedVar(missing));
                }
                Ok(val)
            },
            Expr::Var(ident) => {
                if let Some(val) = self.find_var(&ident, |var| var.clone()) {
                    return Ok(val);
                } else {
                    return Err(Error::UndefinedVar(ident));
                }
            },
            Expr::IIz(name, args) => {
                let mut args_val = Vec::with_capacity(args.len());
                for arg in args {
                    args_val.push(self.eval_expr(arg)?);
                }
                self.call_func(&name, args_val)
            },

            Expr::SumOf(one, two) => self.apply_num(*one, *two, |x, y| x + y, |x, y| x + y),
            Expr::DiffOf(one, two) => self.apply_num(*one, *two, |x, y| x - y, |x, y| x - y),
            Expr::ProduktOf(one, two) => self.apply_num(*one, *two, |x, y| x * y, |x, y| x * y),
            Expr::QuoshuntOf(one, two) => self.apply_num(*one, *two, |x, y| x / y, |x, y| x / y),
            Expr::ModOf(one, two) => self.apply_num(*one, *two, |x, y| x % y, |x, y| x % y),
            Expr::BiggrOf(one, two) => self.apply_num(*one, *two, |x, y| x.max(y), |x, y| x.max(y)),
            Expr::SmallrOf(one, two) => self.apply_num(*one, *two, |x, y| x.min(y), |x, y| x.min(y)),

            Expr::BothOf(one, two) => self.apply_bool(*one, *two, |x, y| x && y),
            Expr::EitherOf(one, two) => self.apply_bool(*one, *two, |x, y| x || y),
            Expr::WonOf(one, two) => self.apply_bool(*one, *two, |x, y| x ^ y),
            Expr::Not(inner) => Ok(Value::Troof(!self.eval_expr(*inner)?.cast_troof())),
            Expr::AllOf(values) => {
                for value in values {
                    if !self.eval_expr(value)?.cast_troof() {
                        return Ok(Value::Troof(false))
                    }
                }
                Ok(Value::Troof(true))
            },
            Expr::AnyOf(values) => {
                for value in values {
                    if self.eval_expr(value)?.cast_troof() {
                        return Ok(Value::Troof(true))
                    }
                }
                Ok(Value::Troof(false))
            },

            Expr::BothSaem(one, two) => self.apply_any(*one, *two, |x, y| x == y),
            Expr::Diffrint(one, two) => self.apply_any(*one, *two, |x, y| x == y),

            Expr::Smoosh(exprs) => {
                let mut result = String::new();
                for expr in exprs {
                    result.push_str(&self.eval_expr(expr)?.cast_yarn().ok_or(Error::InvalidCast)?);
                }
                Ok(Value::Yarn(result))
            }
        }
    }
    pub fn eval(&self, ast: AST) -> Result<Return> {
        match ast {
            AST::IHasA(ident, expr) => {
                let mut vars = self.vars.borrow_mut();
                if vars.contains_key(&ident) {
                    return Err(Error::ShadowVar(ident));
                }
                vars.insert(ident, self.eval_expr(expr)?);
            },
            AST::R(ident, expr) => {
                let val = self.eval_expr(expr)?;
                if self.find_var(&ident, |var| *var = val).is_none() {
                    return Err(Error::UndefinedVar(ident));
                }
            },
            AST::It(expr) => *self.it.borrow_mut() = self.eval_expr(expr)?,
            AST::ORly(yarly, mebbe, nowai) => {
                if self.it.borrow().cast_troof() {
                    return self.scope().eval_all(yarly);
                }
                for (condition, block) in mebbe {
                    if self.eval_expr(condition)?.cast_troof() {
                        return self.scope().eval_all(block);
                    }
                }
                self.scope().eval_all(nowai)?;
            },
            AST::Wtf(omg, omgwtf) => {
                let mut matched = false;
                let it = self.it.borrow();
                for (condition, block) in omg {
                    if matched || *it == self.eval_expr(condition)? {
                        matched = true;
                        match self.scope().eval_all(block)? {
                            Return::None => (),
                            Return::Gtfo => return Ok(Return::None),
                            val @ Return::Value(_) => return Ok(val)
                        }
                    }
                }
                self.scope().eval_all(omgwtf)?;
            },
            AST::ImInYr(operation, var, condition, block) => {
                let mut scope = self.scope();
                scope.vars.borrow_mut().insert(var.clone(), Value::Numbr(0));
                while condition.is_none() || scope.eval_expr(condition.clone().unwrap())?.cast_troof() {
                    match scope.eval_all(block.clone())? {
                        Return::None => (),
                        Return::Gtfo => return Ok(Return::None),
                        val @ Return::Value(_) => return Ok(val)
                    }
                    let val = scope.vars.borrow_mut()
                        .entry(var.clone())
                        .or_insert(Value::Numbr(0))
                        .clone();
                    let val = match operation {
                        Operation::Uppin => Value::Numbr(val.cast_numbr().ok_or(Error::InvalidCastLoop)? + 1),
                        Operation::Nerfin => Value::Numbr(val.cast_numbr().ok_or(Error::InvalidCastLoop)? - 1),
                        Operation::IIz(ref name) => scope.call_func(&name, vec![val])?
                    };
                    *scope.vars.borrow_mut().get_mut(&var).unwrap() = val;
                }
            },
            AST::HowIzI(name, args, block) => {
                self.funcs.borrow_mut().insert(name.clone(), Function {
                    args: args,
                    block: block
                });
            },

            AST::Gtfo => return Ok(Return::Gtfo),
            AST::FoundYr(expr) => return Ok(Return::Value(self.eval_expr(expr)?)),

            AST::Visible(exprs, newline) => {
                let mut result = String::new();
                for expr in exprs {
                    result.push_str(&self.eval_expr(expr)?.cast_yarn().ok_or(Error::InvalidCast)?);
                }
                let stdout = self.top_parent().stdout.as_ref().expect("No stdout handle on Scope");
                let mut stdout = stdout.borrow_mut();
                stdout.write_all(result.as_bytes()).unwrap();
                if newline {
                    stdout.write_all(b"\n").unwrap();
                } else {
                    stdout.flush().unwrap();
                }
            },
            AST::Gimmeh(ident) => {
                let stdin = self.top_parent().stdin.as_ref().expect("No stdout handle on Scope");
                let mut stdin = stdin.borrow_mut();

                let mut text = String::new();
                stdin.read_line(&mut text).unwrap();

                let text = text.trim().to_string();
                self.vars.borrow_mut().insert(ident, Value::Yarn(text));
            }
        }
        Ok(Return::None)
    }
    pub fn eval_all<I: IntoIterator<Item = AST>>(&self, asts: I) -> Result<Return> {
        for line in asts.into_iter() {
            match self.eval(line)? {
                Return::None => (),
                val => return Ok(val)
            }
        }
        Ok(Return::None)
    }
}
