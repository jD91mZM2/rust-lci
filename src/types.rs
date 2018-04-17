use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq)]
pub enum Interpolate {
    Str(String),
    Var(String)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Noob,
    Yarn(String),
    YarnRaw(Vec<Interpolate>),
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
    pub fn cast_yarn(&self) -> Option<Cow<str>> {
        match *self {
            Value::Noob => None,
            Value::Yarn(ref inner) => Some(Cow::Borrowed(inner)),
            Value::YarnRaw(_) => panic!("yarn not interpolated yet"),
            Value::Numbr(n) => Some(Cow::Owned(n.to_string())),
            Value::Numbar(n) => Some(Cow::Owned(n.to_string())),
            Value::Troof(true) => Some(Cow::Borrowed("WIN")),
            Value::Troof(false) => Some(Cow::Borrowed("FAIL"))
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
    pub fn is_numbr(&self) -> bool {
        match *self {
            Value::Yarn(ref inner) => inner.parse::<u64>().is_ok(),
            Value::YarnRaw(_) => panic!("yarn not interpolated yet"),
            Value::Numbr(_) => true,
            _ => false
        }
    }
    pub fn is_numbar(&self) -> bool {
        match *self {
            Value::Yarn(ref inner) => !self.is_numbr() && inner.parse::<f64>().is_ok(),
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
    /// Check if the values are equal (used by the BOTH SAEM operator).
    /// This does auto-coercion, unlike the PartialEq trait implementation.
    pub fn equals(&self, other: &Self) -> bool {
        if let Value::YarnRaw(_) = *self  { panic!("yarn not interpolated yet"); }
        if let Value::YarnRaw(_) = *other { panic!("yarn not interpolated yet"); }

        if let Value::Noob = *self {
            if let Value::Noob = *other {
                return true;
            }
        }
        if let Value::Troof(b) = *self {
            if let Value::Troof(b2) = *other {
                return b == b2;
            }
        }
        if self.is_numbar() || other.is_numbar() {
            self.cast_numbar() == other.cast_numbar()
        } else if self.is_numbr() || other.is_numbr() {
            self.cast_numbr() == other.cast_numbr()
        } else {
            self.cast_yarn() == other.cast_yarn()
        }
    }
    /// Interpolate a YARN value at evaluation time.
    /// This does nothing if it's not a YARN or if it already has
    /// been interpolated.
    /// Returns any missing variable in the interpolation, if any.
    pub fn interpolate<F>(&mut self, lookup: F) -> Option<String>
        where F: Fn(&str) -> Option<Value>
    {
        let mut string_ = None;
        if let Value::YarnRaw(ref parts) = *self {
            let mut capacity = 0;
            for part in parts {
                if let Interpolate::Str(ref part) = *part {
                    capacity += part.len();
                }
            }
            let mut string = String::with_capacity(capacity);
            for part in parts {
                let mut _val = None;
                string.push_str(&match *part {
                    Interpolate::Str(ref part) => Cow::Borrowed(&**part),
                    Interpolate::Var(ref var) => match lookup(var).and_then(|val| {
                        _val = Some(val);
                        _val.as_ref().unwrap().cast_yarn()
                    }) {
                        Some(val) => val,
                        None => return Some(var.clone())
                    }
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
