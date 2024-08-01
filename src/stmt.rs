use crate::{
    expr::Expr,
    interpreter::{Interpreter, RuntimeError, Scope},
    tokenizer::Token,
};

#[derive(Debug)]
pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Expression {
        expr: Expr,
    },
    Print {
        expr: Expr,
    },
    Var {
        name: Token,
        initialiser: Expr,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
}

impl Stmt {
    pub fn interpret(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self {
            Stmt::Expression { expr } => {
                expr.evaluate(interpreter)?;
                Ok(())
            }
            Stmt::Print { expr } => {
                println!("{}", expr.evaluate(interpreter)?);
                Ok(())
            }
            Stmt::Block { statements } => {
                interpreter.scopes.push(Scope::new());
                for statement in statements {
                    statement.interpret(interpreter)?;
                }
                interpreter.scopes.pop();
                Ok(())
            }
            Stmt::Var { name, initialiser } => {
                let value = initialiser.evaluate(interpreter)?;
                let current_scope = interpreter.scopes.last_mut().unwrap();
                current_scope.insert(name.lexeme.clone(), value);
                Ok(())
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if condition.evaluate(interpreter)?.is_truthy() {
                    then_branch.interpret(interpreter)?;
                } else if let Some(else_branch) = else_branch {
                    else_branch.interpret(interpreter)?;
                }
                Ok(())
            }
            Stmt::While { condition, body } => {
                while condition.evaluate(interpreter)?.is_truthy() {
                    body.interpret(interpreter)?;
                }
                Ok(())
            }
        }
    }
}
