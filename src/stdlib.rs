use std::{
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{interpreter::Interpreter, value::LoxValue};

pub fn clock(_: &mut Interpreter, _: Vec<Rc<LoxValue>>) -> Rc<LoxValue> {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    Rc::new(LoxValue::Number(
        since_the_epoch.as_millis() as f64 / 1000.0,
    ))
}
