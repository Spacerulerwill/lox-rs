use crate::value::LoxValue;
use lazy_static::lazy_static;
use std::{collections::HashMap, iter::Peekable, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

lazy_static! {
    static ref RESERVED_IDENTIFIER_HASHMAP: HashMap<&'static str, (TokenType, LoxValue)> = {
        let mut m = HashMap::new();
        m.insert("and", (TokenType::And, LoxValue::Nil));
        m.insert("class", (TokenType::Class, LoxValue::Nil));
        m.insert("else", (TokenType::Else, LoxValue::Nil));
        m.insert("false", (TokenType::False, LoxValue::Boolean(false)));
        m.insert("for", (TokenType::For, LoxValue::Nil));
        m.insert("fun", (TokenType::Fun, LoxValue::Nil));
        m.insert("if", (TokenType::If, LoxValue::Nil));
        m.insert("nil", (TokenType::Nil, LoxValue::Nil));
        m.insert("or", (TokenType::Or, LoxValue::Nil));
        m.insert("print", (TokenType::Print, LoxValue::Nil));
        m.insert("return", (TokenType::Return, LoxValue::Nil));
        m.insert("super", (TokenType::Super, LoxValue::Nil));
        m.insert("this", (TokenType::This, LoxValue::Nil));
        m.insert("true", (TokenType::True, LoxValue::Boolean(true)));
        m.insert("var", (TokenType::Var, LoxValue::Nil));
        m.insert("while", (TokenType::While, LoxValue::Nil));
        m
    };
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: LoxValue,
    pub lexeme: String,
    pub position: TokenPosition,
}

#[derive(Debug)]
pub enum TokenizerError {
    UnexpectedCharacter { char: char, position: TokenPosition },
    UnterminatedStringLiteral { position: TokenPosition },
}

#[derive(Debug, Clone, Copy)]
pub struct TokenPosition {
    pub idx: usize,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    input: &'a str,
    iter: Peekable<Chars<'a>>,
    start_pos: TokenPosition,
    current_pos: TokenPosition,
    tabsize: usize,
    pub tokens: Vec<Token>,
}

impl<'a> Tokenizer<'a> {
    pub fn tokenize(input: &'a str, tabsize: usize) -> Result<Tokenizer, TokenizerError> {
        let mut tokenizer = Tokenizer {
            input: input,
            iter: input.chars().peekable(),
            start_pos: TokenPosition {
                idx: 0,
                line: 1,
                col: 1,
            },
            current_pos: TokenPosition {
                idx: 0,
                line: 1,
                col: 1,
            },
            tabsize: tabsize,
            tokens: Vec::new(),
        };
        tokenizer.internal_tokenize()?;
        Ok(tokenizer)
    }

    fn internal_tokenize(&mut self) -> Result<(), TokenizerError> {
        while let Some(&ch) = self.iter.peek() {
            // Skip all whitespace characters
            if ch.is_whitespace() {
                self.next();
                self.start_pos = self.current_pos;
                continue;
            }
            // Tokenize based on char found
            match ch {
                '(' => self.tokenize_single_char_token(TokenType::LeftParen),
                ')' => self.tokenize_single_char_token(TokenType::RightParen),
                '{' => self.tokenize_single_char_token(TokenType::LeftBrace),
                '}' => self.tokenize_single_char_token(TokenType::RightBrace),
                ',' => self.tokenize_single_char_token(TokenType::Comma),
                '.' => self.tokenize_single_char_token(TokenType::Dot),
                '-' => self.tokenize_single_char_token(TokenType::Minus),
                '+' => self.tokenize_single_char_token(TokenType::Plus),
                ';' => self.tokenize_single_char_token(TokenType::Semicolon),
                '*' => self.tokenize_single_char_token(TokenType::Star),
                '!' => self.tokenize_potential_double_char_token(
                    '=',
                    TokenType::BangEqual,
                    TokenType::Bang,
                ),
                '=' => self.tokenize_potential_double_char_token(
                    '=',
                    TokenType::EqualEqual,
                    TokenType::Equal,
                ),
                '>' => self.tokenize_potential_double_char_token(
                    '=',
                    TokenType::GreaterEqual,
                    TokenType::Greater,
                ),
                '<' => self.tokenize_potential_double_char_token(
                    '=',
                    TokenType::LessEqual,
                    TokenType::Less,
                ),
                '/' => self.handle_slash(),
                '"' => self.tokenize_string_literal()?,
                '0'..='9' => self.tokenizer_number()?,
                'a'..='z' | 'A'..='Z' => self.tokenize_identifier(),
                _ => {
                    return Err(TokenizerError::UnexpectedCharacter {
                        char: ch,
                        position: self.current_pos,
                    })
                }
            }
        }

        // Ugly hack to get EOF token working
        self.add_token(TokenType::Eof);
        self.tokens.last_mut().unwrap().lexeme = String::from("EOF");
        Ok(())
    }

    fn tokenize_single_char_token(&mut self, token_type: TokenType) {
        self.next();
        self.add_token(token_type);
    }

    fn tokenize_potential_double_char_token(
        &mut self,
        next: char,
        success: TokenType,
        fail: TokenType,
    ) {
        self.next();
        if let Some(&ch) = self.iter.peek() {
            if ch == next {
                self.next();
                self.add_token(success);
                return;
            }
        }
        self.add_token(fail);
    }

    fn handle_slash(&mut self) {
        self.next();
        if let Some(&ch) = self.iter.peek() {
            match ch {
                '/' => {
                    self.next();
                    while let Some(&ch) = self.iter.peek() {
                        self.next();
                        if ch == '\n' {
                            break;
                        }
                    }
                    self.start_pos = self.current_pos;
                }
                _ => self.add_token(TokenType::Slash),
            }
        } else {
            self.add_token(TokenType::Slash)
        }
    }

    fn tokenize_string_literal(&mut self) -> Result<(), TokenizerError> {
        self.next();
        let mut value = String::new();
        loop {
            if let Some(&ch) = self.iter.peek() {
                self.next();
                if ch == '"' {
                    break;
                } else {
                    value.push(ch);
                }
            } else {
                return Err(TokenizerError::UnterminatedStringLiteral {
                    position: self.start_pos,
                });
            }
        }
        self.add_token_with_value(TokenType::String, LoxValue::String(Box::new(value)));
        Ok(())
    }

    fn tokenizer_number(&mut self) -> Result<(), TokenizerError> {
        // Consume digits before decimal point
        let mut double_string = self.consume_digits();
        // Consume decimal point if exists,
        let iter_save = self.iter.clone();
        let start_pos_save = self.start_pos.clone();
        if self.is_char_next('.') {
            self.next();
            let post_dot_digit = self.consume_digits();
            if post_dot_digit.is_empty() {
                self.iter = iter_save;
                self.start_pos = start_pos_save;
            } else {
                double_string.push('.');
                double_string.push_str(post_dot_digit.as_str());
            }
        }
        self.add_token_with_value(
            TokenType::Number,
            LoxValue::Number(double_string.parse().unwrap()),
        );
        Ok(())
    }

    fn tokenize_identifier(&mut self) {
        let identifier = self.consume_alphanumeric();
        if let Some((reserved_identifier, value)) =
            RESERVED_IDENTIFIER_HASHMAP.get(identifier.as_str())
        {
            self.add_token_with_value(reserved_identifier.clone(), value.clone())
        } else {
            self.add_token_with_value(TokenType::Identifier, LoxValue::Nil)
        }
    }

    fn next(&mut self) -> Option<char> {
        if let Some(ch) = self.iter.next() {
            match ch {
                '\n' => {
                    self.current_pos.line += 1;
                    self.current_pos.col = 1;
                }
                '\t' => self.current_pos.col += self.tabsize,
                _ => self.current_pos.col += 1,
            }
            self.current_pos.idx += 1;
            return Some(ch);
        }
        return None;
    }

    fn is_char_next(&mut self, expected: char) -> bool {
        if let Some(&ch) = self.iter.peek() {
            if ch == expected {
                return true;
            }
        }
        return false;
    }

    fn consume_digits(&mut self) -> String {
        let mut string = String::new();
        while let Some(&ch) = self.iter.peek() {
            if !ch.is_digit(10) {
                break;
            }
            string.push(ch);
            self.next();
        }
        string
    }

    fn consume_alphanumeric(&mut self) -> String {
        let mut string = String::new();
        while let Some(&ch) = self.iter.peek() {
            if !ch.is_alphanumeric() {
                break;
            }
            string.push(ch);
            self.next();
        }
        string
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_with_value(token_type, LoxValue::Nil);
    }

    fn add_token_with_value(&mut self, token_type: TokenType, value: LoxValue) {
        self.tokens.push(Token {
            token_type: token_type,
            value: value,
            lexeme: self.input[self.start_pos.idx..self.current_pos.idx].to_string(),
            position: self.current_pos,
        });
        self.start_pos = self.current_pos;
    }
}
