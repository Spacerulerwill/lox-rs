use std::borrow::Borrow;
use std::rc::Rc;

use crate::function::Callable;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::tokenizer::{Token, TokenType};
use crate::value::LoxValue;

#[derive(Debug)]
pub enum Expr {
    Assign {
        name: Token,
        new_value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        literal: Rc<LoxValue>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Identifier {
        name: Token,
    },
}

impl Expr {
    pub fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Rc<LoxValue>, RuntimeError> {
        match self {
            Expr::Assign { name, new_value } => {
                let new_value = new_value.evaluate(interpreter)?;
                let mut updated = false;
                for scope in interpreter.scopes.iter_mut().rev() {
                    if let Some(val) = scope.get_mut(&name.lexeme) {
                        *val = new_value.clone();
                        updated = true;
                        break;
                    }
                }

                if let Some(val) = interpreter.globals.get_mut(&name.lexeme) {
                    *val = new_value.clone();
                    updated = true;
                }

                if updated {
                    Ok(new_value)
                } else {
                    Err(RuntimeError::UndefinedIdentifier(name.clone()))
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_operand = left.evaluate(interpreter)?;
                let right_operand = right.evaluate(interpreter)?;

                let left_borrowed: &LoxValue = left_operand.borrow();
                let right_borrowed: &LoxValue = right_operand.borrow();

                match operator.token_type {
                    TokenType::Plus => match (left_borrowed, right_borrowed) {
                        (LoxValue::Number(num1), LoxValue::Number(num2)) => {
                            Ok(Rc::new(LoxValue::Number(num1 + num2)))
                        }
                        (LoxValue::String(string1), LoxValue::String(string2)) => Ok(Rc::new(
                            LoxValue::String(Box::new(*string1.clone() + &*string2)),
                        )),
                        _ => Err(RuntimeError::BinaryExprTypeError(
                            left_operand,
                            operator.clone(),
                            right_operand,
                        )),
                    },
                    TokenType::Minus => match (left_borrowed, right_borrowed) {
                        (LoxValue::Number(num1), LoxValue::Number(num2)) => {
                            Ok(Rc::new(LoxValue::Number(num1 - num2)))
                        }
                        _ => Err(RuntimeError::BinaryExprTypeError(
                            left_operand,
                            operator.clone(),
                            right_operand,
                        )),
                    },
                    TokenType::Star => match (left_borrowed, right_borrowed) {
                        (LoxValue::Number(num1), LoxValue::Number(num2)) => {
                            Ok(Rc::new(LoxValue::Number(num1 * num2)))
                        }
                        _ => Err(RuntimeError::BinaryExprTypeError(
                            left_operand,
                            operator.clone(),
                            right_operand,
                        )),
                    },
                    TokenType::Slash => match (left_borrowed, right_borrowed) {
                        (LoxValue::Number(num1), LoxValue::Number(num2)) => {
                            Ok(Rc::new(LoxValue::Number(num1 / num2)))
                        }
                        _ => Err(RuntimeError::BinaryExprTypeError(
                            left_operand,
                            operator.clone(),
                            right_operand,
                        )),
                    },
                    TokenType::EqualEqual => match (left_borrowed, right_borrowed) {
                        (LoxValue::String(string1), LoxValue::String(string2)) => {
                            Ok(Rc::new(LoxValue::Boolean(string1 == string2)))
                        }
                        (LoxValue::Number(number1), LoxValue::Number(number2)) => {
                            Ok(Rc::new(LoxValue::Boolean(number1 == number2)))
                        }
                        (LoxValue::Boolean(bool1), LoxValue::Boolean(bool2)) => {
                            Ok(Rc::new(LoxValue::Boolean(bool1 == bool2)))
                        }
                        (LoxValue::Nil, LoxValue::Nil) => Ok(Rc::new(LoxValue::Boolean(true))),
                        _ => Ok(Rc::new(LoxValue::Boolean(false))),
                    },
                    TokenType::BangEqual => match (left_borrowed, right_borrowed) {
                        (LoxValue::String(string1), LoxValue::String(string2)) => {
                            Ok(Rc::new(LoxValue::Boolean(string1 != string2)))
                        }
                        (LoxValue::Number(number1), LoxValue::Number(number2)) => {
                            Ok(Rc::new(LoxValue::Boolean(number1 != number2)))
                        }
                        (LoxValue::Boolean(bool1), LoxValue::Boolean(bool2)) => {
                            Ok(Rc::new(LoxValue::Boolean(bool1 != bool2)))
                        }
                        (LoxValue::Nil, LoxValue::Nil) => Ok(Rc::new(LoxValue::Boolean(false))),
                        _ => Ok(Rc::new(LoxValue::Boolean(true))),
                    },
                    TokenType::Greater => match (left_borrowed, right_borrowed) {
                        (LoxValue::String(string1), LoxValue::String(string2)) => {
                            Ok(Rc::new(LoxValue::Boolean(string1 > string2)))
                        }
                        (LoxValue::Number(num1), LoxValue::Number(num2)) => {
                            Ok(Rc::new(LoxValue::Boolean(num1 > num2)))
                        }
                        _ => Err(RuntimeError::BinaryExprTypeError(
                            left_operand,
                            operator.clone(),
                            right_operand,
                        )),
                    },
                    TokenType::GreaterEqual => match (left_borrowed, right_borrowed) {
                        (LoxValue::String(string1), LoxValue::String(string2)) => {
                            Ok(Rc::new(LoxValue::Boolean(string1 >= string2)))
                        }
                        (LoxValue::Number(num1), LoxValue::Number(num2)) => {
                            Ok(Rc::new(LoxValue::Boolean(num1 >= num2)))
                        }
                        _ => Err(RuntimeError::BinaryExprTypeError(
                            left_operand,
                            operator.clone(),
                            right_operand,
                        )),
                    },
                    TokenType::Less => match (left_borrowed, right_borrowed) {
                        (LoxValue::String(string1), LoxValue::String(string2)) => {
                            Ok(Rc::new(LoxValue::Boolean(string1 < string2)))
                        }
                        (LoxValue::Number(num1), LoxValue::Number(num2)) => {
                            Ok(Rc::new(LoxValue::Boolean(num1 < num2)))
                        }
                        _ => Err(RuntimeError::BinaryExprTypeError(
                            left_operand,
                            operator.clone(),
                            right_operand,
                        )),
                    },
                    TokenType::LessEqual => match (left_borrowed, right_borrowed) {
                        (LoxValue::String(string1), LoxValue::String(string2)) => {
                            Ok(Rc::new(LoxValue::Boolean(string1 <= string2)))
                        }
                        (LoxValue::Number(num1), LoxValue::Number(num2)) => {
                            Ok(Rc::new(LoxValue::Boolean(num1 <= num2)))
                        }
                        _ => Err(RuntimeError::BinaryExprTypeError(
                            left_operand,
                            operator.clone(),
                            right_operand,
                        )),
                    },
                    _ => panic!(
                        "Invalid operator for binary expression: {:?}",
                        operator.token_type
                    ),
                }
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left_operand = left.evaluate(interpreter)?;
                match operator.token_type {
                    TokenType::And => {
                        if !left_operand.is_truthy() {
                            Ok(left_operand)
                        } else {
                            Ok(right.evaluate(interpreter)?)
                        }
                    }
                    TokenType::Or => {
                        if left_operand.is_truthy() {
                            Ok(left_operand)
                        } else {
                            Ok(right.evaluate(interpreter)?)
                        }
                    }
                    _ => panic!(
                        "Invalid operator for logical binary expression: {:?}",
                        operator.token_type
                    ),
                }
            }
            Expr::Grouping { expression } => expression.evaluate(interpreter),
            Expr::Literal { literal } => Ok(literal.clone()),
            Expr::Unary { operator, right } => {
                let operand = right.evaluate(interpreter)?;
                let operand_borrowed: &LoxValue = operand.borrow();
                match operator.token_type {
                    TokenType::Bang => match operand_borrowed {
                        LoxValue::Boolean(bool) => Ok(Rc::new(LoxValue::Boolean(!bool))),
                        _ => Err(RuntimeError::UnaryExprTypeError(operand, operator.clone())),
                    },
                    TokenType::Minus => match operand_borrowed {
                        LoxValue::Number(num) => Ok(Rc::new(LoxValue::Number(-num))),
                        _ => Err(RuntimeError::UnaryExprTypeError(operand, operator.clone())),
                    },
                    _ => panic!(
                        "Invalid operator for binary expression: {:?}",
                        operator.token_type
                    ),
                }
            }
            Expr::Identifier { name } => {
                for scope in interpreter.scopes.iter_mut().rev() {
                    if let Some(value) = scope.get(&name.lexeme) {
                        return Ok(value.clone());
                    }
                }
                if let Some(value) = interpreter.globals.get(&name.lexeme) {
                    return Ok(value.clone());
                }
                return Err(RuntimeError::UndefinedIdentifier(name.clone()));
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = callee.evaluate(interpreter)?;
                let mut evaluated_arguments = Vec::new();
                for argument in arguments {
                    evaluated_arguments.push(argument.evaluate(interpreter)?);
                }
                if let LoxValue::NativeFunction(f) = callee.borrow() {
                    if evaluated_arguments.len() != f.arity() {
                        return Err(RuntimeError::IncorrectArgumentCount {
                            left_paren: paren.clone(),
                            expected: f.arity(),
                            recieved: evaluated_arguments.len(),
                        });
                    }
                    let value = f.call(interpreter, evaluated_arguments);
                    return Ok(value);
                }
                Err(RuntimeError::InvalidCallable(paren.clone(), callee))
            }
        }
    }
}
