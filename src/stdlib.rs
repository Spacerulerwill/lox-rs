use std::{
    io::{stdin, stdout, Write}, rc::Rc, time::{SystemTime, UNIX_EPOCH}
};

use crate::{interpreter::{Interpreter, RuntimeError}, value::LoxValue};

pub fn clock(_: &mut Interpreter, _: Vec<Rc<LoxValue>>) -> Result<Rc<LoxValue>, RuntimeError> {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    Ok(Rc::new(LoxValue::Number(
        since_the_epoch.as_millis() as f64 / 1000.0,
    )))
}

pub fn input(_: &mut Interpreter, args: Vec<Rc<LoxValue>>) -> Result<Rc<LoxValue>, RuntimeError> {
    let message = args.get(0).unwrap();
    let mut handle = stdout().lock(); 
    write!(handle, "{}", message).unwrap();
    handle.flush().unwrap();
    let mut line = String::new();
    stdin().read_line(&mut line).unwrap();
    Ok(Rc::new(LoxValue::String(Box::new(line.trim().to_string()))))
}
