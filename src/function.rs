use std::rc::Rc;

use crate::{interpreter::Interpreter, value::LoxValue};

pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Rc<LoxValue>>) -> Rc<LoxValue>;
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    pub arity: usize,
    pub callable: fn(&mut Interpreter, Vec<Rc<LoxValue>>) -> Rc<LoxValue>,
}

impl Callable for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Rc<LoxValue>>) -> Rc<LoxValue> {
        (self.callable)(interpreter, args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}
