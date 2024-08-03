/*
program → declaration* EOF ;

declaration → varDecl
| statement ;

varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;

statement → exprStmt
            | ifStmt
            | forStmt
            | whileStmt
            | printStmt
            | block ;

ifStmt → "if" "(" expression ")" statement
    ( "else" statement )? ;
 whileStmt → "while" "(" expression ")" statement ;
 forStmt → "for" "(" ( varDecl | exprStmt | ";" )
    expression? ";"
    expression? ")" statement ;
exprStmt → expression ";" ;
printStmt → "print" expression ";"
block → "{" declaration* "}" ;

expression → assignment ;
assignment → IDENTIFIER "=" assignment
| logic_or ;
logic_or → logic_and ( "or" logic_and )* ;
logic_and → equality ( "and" equality )* ;
equality → comparison ( ( "!=" | "==" ) comparison )* ;
comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term → factor ( ( "-" | "+" ) factor )* ;
factor → unary ( ( "/" | "*" ) unary )* ;
unary → ( "!" | "-" ) unary | call ;
call → primary ( "(" arguments? ")" )* ;
primary → NUMBER | STRING | "true" | "false" | "nil"
| "(" expression ")" | IDENTIFIER;

arguments → expression ( "," expression )* ;
*/

use crate::{
    expr::Expr,
    stmt::Stmt,
    tokenizer::{Token, TokenPosition, TokenType},
    value::LoxValue,
};
use std::{iter::Peekable, rc::Rc, slice::Iter};

#[derive(Debug)]
pub enum ParserError {
    ExpectedExpression(TokenPosition),
    ExpectedToken { expected: TokenType, found: Token },
    InvalidAssignmentTarget(Token),
    TooManyArguments { left_paren: Token },
}

// Recursive descent parser based on grammar above
#[derive(Debug)]
pub struct Parser<'a> {
    iter: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: &'a Vec<Token>) -> Result<Vec<Stmt>, ParserError> {
        let mut parser = Parser {
            iter: tokens.into_iter().peekable(),
        };
        let mut statements = Vec::new();
        while parser.iter.peek().unwrap().token_type != TokenType::Eof {
            statements.push(parser.declaration()?);
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(&[TokenType::Var]).is_some() {
            return Ok(self.var_declaration()?);
        }
        return Ok(self.statement()?);
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name_token = self.consume(TokenType::Identifier)?;

        let initializer;
        if self.matches(&[TokenType::Equal]).is_some() {
            initializer = self.expression()?;
        } else {
            initializer = Expr::Literal {
                literal: Rc::new(LoxValue::Nil),
            }
        };

        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Var {
            name: name_token,
            initialiser: initializer,
        })
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(&[TokenType::Print]).is_some() {
            return Ok(self.print_statement()?);
        }
        if self.matches(&[TokenType::LeftBrace]).is_some() {
            return Ok(self.block_statement()?);
        }
        if self.matches(&[TokenType::If]).is_some() {
            return Ok(self.if_statement()?);
        }
        if self.matches(&[TokenType::While]).is_some() {
            return Ok(self.while_statement()?);
        }
        if self.matches(&[TokenType::For]).is_some() {
            return Ok(self.for_statement()?);
        }
        Ok(self.expression_statement()?)
    }

    fn block_statement(&mut self) -> Result<Stmt, ParserError> {
        let mut statements = Vec::new();
        loop {
            let token = self.iter.peek().unwrap();
            match token.token_type {
                TokenType::RightBrace => {
                    self.iter.next();
                    break;
                }
                TokenType::Eof => {
                    return Err(ParserError::ExpectedToken {
                        expected: TokenType::RightBrace,
                        found: (*token).clone(),
                    })
                }
                _ => {}
            }
            statements.push(self.declaration()?);
        }
        Ok(Stmt::Block {
            statements: statements,
        })
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        return Ok(Stmt::Print { expr: expr });
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        return Ok(Stmt::Expression { expr: expr });
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(TokenType::LeftParen)?;
        let condition_expression = self.expression()?;
        self.consume(TokenType::RightParen)?;
        let then_statement = Box::new(self.statement()?);
        let else_statement;
        if self.matches(&[TokenType::Else]).is_some() {
            else_statement = Some(Box::new(self.statement()?));
        } else {
            else_statement = None;
        }
        Ok(Stmt::If {
            condition: condition_expression,
            then_branch: then_statement,
            else_branch: else_statement,
        })
    }

    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(TokenType::LeftParen)?;
        let condition_expression = self.expression()?;
        self.consume(TokenType::RightParen)?;
        let loop_body = Box::new(self.statement()?);
        Ok(Stmt::While {
            condition: condition_expression,
            body: loop_body,
        })
    }

    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(TokenType::LeftParen)?;

        // Handle initializer
        let initializer = if self.matches(&[TokenType::Semicolon]).is_some() {
            None
        } else if self.matches(&[TokenType::Var]).is_some() {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        // Handle condition
        let condition = if self.check(TokenType::Semicolon) {
            Some(Expr::Literal {
                literal: Rc::new(LoxValue::Boolean(true)),
            })
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenType::Semicolon)?;

        // Handle incrementer
        let incrementer = if self.check(TokenType::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenType::RightParen)?;

        // Handle the body of the loop
        let mut body = self.statement()?;

        // Convert the for loop to a while loop
        if let Some(incrementer) = incrementer {
            body = Stmt::Block {
                statements: vec![body, Stmt::Expression { expr: incrementer }],
            };
        }
        let while_body = Stmt::While {
            condition: condition.unwrap(),
            body: Box::new(body),
        };
        let final_body = if let Some(initializer) = initializer {
            Stmt::Block {
                statements: vec![initializer, while_body],
            }
        } else {
            while_body
        };
        Ok(final_body)
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.logic_or()?;
        if let Some(equals) = self.matches(&[TokenType::Equal]) {
            let value = self.assignment()?;
            match expr {
                Expr::Identifier { name } => {
                    return Ok(Expr::Assign {
                        name: name,
                        new_value: Box::new(value),
                    })
                }
                _ => return Err(ParserError::InvalidAssignmentTarget(equals)),
            }
        }
        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.logic_and()?;
        while let Some(operator) = self.matches(&[TokenType::Or]) {
            let right = self.logic_and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;
        while let Some(operator) = self.matches(&[TokenType::And]) {
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(operator) = self.matches(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;

        while let Some(operator) = self.matches(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;

        while let Some(operator) = self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.matches(&[TokenType::Star, TokenType::Slash]) {
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(operator) = self.matches(&[TokenType::Bang, TokenType::Minus]) {
            let right = self.unary()?;
            return Ok(Expr::Unary {
                operator: operator,
                right: Box::new(right),
            });
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;
        loop {
            if let Some(left_paren) = self.matches(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr, left_paren)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr, left_paren: Token) -> Result<Expr, ParserError> {
        let mut arguments = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(ParserError::TooManyArguments {
                        left_paren: left_paren,
                    });
                }
                arguments.push(self.expression()?);
                if !self.matches(&[TokenType::Comma]).is_some() {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen)?;
        Ok(Expr::Call {
            callee: Box::new(callee),
            paren: left_paren,
            arguments: arguments,
        })
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.matches(&[
            TokenType::True,
            TokenType::False,
            TokenType::Nil,
            TokenType::Number,
            TokenType::String,
        ]) {
            return Ok(Expr::Literal {
                literal: Rc::new(token.value),
            });
        }

        if self.matches(&[TokenType::LeftParen]).is_some() {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen)?;
            return Ok(Expr::Grouping {
                expression: Box::new(expr),
            });
        }

        if let Some(token) = self.matches(&[TokenType::Identifier]) {
            return Ok(Expr::Identifier { name: token });
        }

        let token = self.iter.peek().unwrap();
        return Err(ParserError::ExpectedExpression(token.position));
    }

    fn consume(&mut self, token_type: TokenType) -> Result<Token, ParserError> {
        let token = *self.iter.peek().unwrap();
        if token.token_type == token_type {
            self.iter.next();
            Ok(token.clone())
        } else {
            Err(ParserError::ExpectedToken {
                expected: token_type,
                found: token.clone(),
            })
        }
    }

    fn matches(&mut self, token_types: &[TokenType]) -> Option<Token> {
        if token_types.contains(&self.iter.peek().unwrap().token_type) {
            self.iter.next().cloned()
        } else {
            None
        }
    }

    fn check(&mut self, token_type: TokenType) -> bool {
        self.iter.peek().unwrap().token_type == token_type
    }
}
