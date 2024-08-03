use std::fmt;

use crate::function::NativeFunction;

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    NativeFunction(Box<NativeFunction>),
    String(Box<String>),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl LoxValue {
    /// Useful for error messages
    pub fn get_type_string_repr(&self) -> &str {
        match self {
            LoxValue::String(_) => "string",
            LoxValue::Number(_) => "number",
            LoxValue::Boolean(_) => "boolean",
            LoxValue::Nil => "nil",
            LoxValue::NativeFunction(_) => "function",
        }
    }

    /// Nil and false are falsey, everything else is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxValue::Nil => false,
            LoxValue::Boolean(bool) => *bool,
            _ => true,
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxValue::String(string) => write!(f, "{}", string),
            LoxValue::Number(number) => write!(f, "{}", number),
            LoxValue::Boolean(bool) => write!(f, "{}", bool),
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::NativeFunction(native_function) => {
                write!(f, "<fn at {:?}>", native_function.callable)
            }
        }
    }
}
