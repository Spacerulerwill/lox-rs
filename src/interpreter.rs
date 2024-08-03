use crate::{
    bad_print,
    function::NativeFunction,
    parser::{Parser, ParserError},
    stdlib,
    tokenizer::{Token, Tokenizer, TokenizerError},
    value::LoxValue,
};
use std::{collections::HashMap, rc::Rc};

macro_rules! print_error {
    ($error_type:expr, $line:expr, $col:expr, $($msg:tt)*) => {
        bad_print!("{} :: Line {}, Column {} :: {}", $error_type, $line, $col, format!($($msg)*))
    };
}

pub const DEFAULT_TAB_SIZE: u8 = 4;

#[derive(Debug)]
pub enum RuntimeError {
    BinaryExprTypeError(Rc<LoxValue>, Token, Rc<LoxValue>),
    UnaryExprTypeError(Rc<LoxValue>, Token),
    UndefinedIdentifier(Token),
    InvalidCallable(Token, Rc<LoxValue>),
    IncorrectArgumentCount {
        left_paren: Token,
        expected: usize,
        recieved: usize,
    },
}

pub type Scope = HashMap<String, Rc<LoxValue>>;

#[derive(Debug)]
pub struct Interpreter {
    pub scopes: Vec<Scope>,
    pub globals: Scope,
}

impl<'a> Interpreter {
    pub fn new() -> Self {
        let mut globals = Scope::new();
        globals.insert(
            String::from("clock"),
            Rc::new(LoxValue::NativeFunction(Box::new(NativeFunction {
                arity: 0,
                callable: stdlib::clock,
            }))),
        );
        Self {
            scopes: vec![Scope::new()],
            globals: globals,
        }
    }

    pub fn run(&mut self, source: &str, tabsize: usize) {
        let tokens = match Tokenizer::tokenize(source, tabsize) {
            Ok(tokenizer) => tokenizer.tokens,
            Err(err) => {
                print_tokenizer_err_message(err);
                return;
            }
        };

        let statements = match Parser::parse(&tokens) {
            Ok(statements) => statements,
            Err(err) => {
                print_parser_err_message(err);
                return;
            }
        };

        for statement in statements {
            if let Err(err) = statement.interpret(self) {
                print_runtime_err_message(err);
                return;
            }
        }
    }
}

fn print_tokenizer_err_message(err: TokenizerError) {
    match err {
        TokenizerError::UnexpectedCharacter { char, position } => print_error!(
            "Syntax Error",
            position.line,
            position.col,
            "Unexpected character: {}",
            char
        ),
        TokenizerError::UnterminatedStringLiteral { position } => print_error!(
            "Syntax Error",
            position.line,
            position.col,
            "Unterminated string literal begins here"
        ),
    }
}

fn print_parser_err_message(err: ParserError) {
    match err {
        ParserError::ExpectedExpression(position) => print_error!(
            "Syntax Error",
            position.line,
            position.col,
            "Expected expression"
        ),
        ParserError::ExpectedToken { expected, found } => print_error!(
            "Syntax Error",
            found.position.line,
            found.position.col,
            "Expected {:?}, but found {}",
            expected,
            &found.lexeme,
        ),
        ParserError::InvalidAssignmentTarget(token) => print_error!(
            "Syntax Error",
            token.position.line,
            token.position.col,
            "Invalid assigment target"
        ),
        ParserError::TooManyArguments { left_paren } => print_error!(
            "Syntax Error",
            left_paren.position.line,
            left_paren.position.col,
            "Too many arguments for function"
        ),
    }
}

fn print_runtime_err_message(err: RuntimeError) {
    match err {
        RuntimeError::BinaryExprTypeError(left, operator, right) => print_error!(
            "Runtime Error",
            operator.position.line,
            operator.position.col,
            "Binary operator {} not supported between types {} and {}",
            &operator.lexeme,
            left.get_type_string_repr(),
            right.get_type_string_repr()
        ),
        RuntimeError::UnaryExprTypeError(operand, operator) => print_error!(
            "Runtime Error",
            operator.position.line,
            operator.position.col,
            "Unary operator {} not supported for type {}",
            &operator.lexeme,
            operand.get_type_string_repr()
        ),
        RuntimeError::UndefinedIdentifier(name) => print_error!(
            "Name Error",
            name.position.line,
            name.position.col,
            "No identifier exists with name '{}'",
            &name.lexeme
        ),
        RuntimeError::InvalidCallable(identifier, callee) => print_error!(
            "Type error",
            identifier.position.line,
            identifier.position.col,
            "Attempt to call on non callable type '{}'",
            callee.get_type_string_repr()
        ),
        RuntimeError::IncorrectArgumentCount {
            left_paren,
            expected,
            recieved,
        } => print_error!(
            "Runtime Error",
            left_paren.position.line,
            left_paren.position.col,
            "Expected {expected} arguments, but recieved {recieved}"
        ),
    }
}
