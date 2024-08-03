use std::rc::Rc;

use crate::{interpreter::{Interpreter, RuntimeError}, value::LoxValue};

pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Rc<LoxValue>>) -> Result<Rc<LoxValue>, RuntimeError>;
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    pub arity: usize,
    pub callable: fn(&mut Interpreter, Vec<Rc<LoxValue>>) -> Result<Rc<LoxValue>, RuntimeError>
}

impl Callable for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Rc<LoxValue>>) -> Result<Rc<LoxValue>, RuntimeError> {
        Ok((self.callable)(interpreter, args)?)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}
