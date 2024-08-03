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
        let current_pos_save = self.current_pos.clone();
        if self.is_char_next('.') {
            self.next();
            let post_dot_digit = self.consume_digits();
            if post_dot_digit.is_empty() {
                self.iter = iter_save;
                self.current_pos = current_pos_save;
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
            self.current_pos.idx += ch.len_utf8();
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
            position: self.start_pos,
        });
        self.start_pos = self.current_pos;
    }
}

#[cfg(test)]
mod tests {
    use crate::{interpreter::DEFAULT_TAB_SIZE, tokenizer::Tokenizer};

    use super::*;

    macro_rules! assert_err {
        ($input:expr, $expected_error:pat) => {
            assert!(matches!($input, Err($expected_error)))
        };
    }

    fn extract_token_types_and_values(tokens: Vec<Token>) -> Vec<(TokenType, LoxValue)> {
        tokens
            .into_iter()
            .map(|token: Token| (token.token_type, token.value))
            .collect()
    }

    fn extract_token_types(tokens: Vec<Token>) -> Vec<TokenType> {
        tokens.into_iter().map(|token| token.token_type).collect()
    }

    fn extract_token_positions(tokens: Vec<Token>) -> Vec<(usize, usize, usize)> {
        tokens
            .into_iter()
            .map(|token| (token.position.idx, token.position.line, token.position.col))
            .collect()
    }

    fn extract_token_lexemes(tokens: Vec<Token>) -> Vec<String> {
        tokens.into_iter().map(|token| token.lexeme).collect()
    }

    fn token_type_check(input: &str, expected: &[TokenType]) {
        let tokenizer = Tokenizer::tokenize(input, DEFAULT_TAB_SIZE as usize).unwrap();
        assert_eq!(extract_token_types(tokenizer.tokens), expected);
    }

    fn token_type_checks(test_cases: &[(&str, Vec<TokenType>)]) {
        for (input, expected) in test_cases {
            token_type_check(input, expected)
        }
    }

    #[test]
    fn test_all_valid_tokens_isolated() {
        for (input, result) in [
            ("(", (TokenType::LeftParen, LoxValue::Nil)),
            (")", (TokenType::RightParen, LoxValue::Nil)),
            ("{", (TokenType::LeftBrace, LoxValue::Nil)),
            ("}", (TokenType::RightBrace, LoxValue::Nil)),
            (",", (TokenType::Comma, LoxValue::Nil)),
            (".", (TokenType::Dot, LoxValue::Nil)),
            ("-", (TokenType::Minus, LoxValue::Nil)),
            ("+", (TokenType::Plus, LoxValue::Nil)),
            (";", (TokenType::Semicolon, LoxValue::Nil)),
            ("/", (TokenType::Slash, LoxValue::Nil)),
            ("*", (TokenType::Star, LoxValue::Nil)),
            ("!", (TokenType::Bang, LoxValue::Nil)),
            ("!=", (TokenType::BangEqual, LoxValue::Nil)),
            ("=", (TokenType::Equal, LoxValue::Nil)),
            ("==", (TokenType::EqualEqual, LoxValue::Nil)),
            (">", (TokenType::Greater, LoxValue::Nil)),
            (">=", (TokenType::GreaterEqual, LoxValue::Nil)),
            ("<", (TokenType::Less, LoxValue::Nil)),
            ("<=", (TokenType::LessEqual, LoxValue::Nil)),
            ("bruh", (TokenType::Identifier, LoxValue::Nil)),
            (
                "\"hello\"",
                (
                    TokenType::String,
                    LoxValue::String(Box::new(String::from("hello"))),
                ),
            ),
            (
                "\"\nhello\n\"",
                (
                    TokenType::String,
                    LoxValue::String(Box::new(String::from("\nhello\n"))),
                ),
            ),
            ("3.141", (TokenType::Number, LoxValue::Number(3.141))),
            ("3", (TokenType::Number, LoxValue::Number(3.0))),
            ("and", (TokenType::And, LoxValue::Nil)),
            ("class", (TokenType::Class, LoxValue::Nil)),
            ("else", (TokenType::Else, LoxValue::Nil)),
            ("false", (TokenType::False, LoxValue::Boolean(false))),
            ("fun", (TokenType::Fun, LoxValue::Nil)),
            ("for", (TokenType::For, LoxValue::Nil)),
            ("if", (TokenType::If, LoxValue::Nil)),
            ("nil", (TokenType::Nil, LoxValue::Nil)),
            ("or", (TokenType::Or, LoxValue::Nil)),
            ("print", (TokenType::Print, LoxValue::Nil)),
            ("return", (TokenType::Return, LoxValue::Nil)),
            ("super", (TokenType::Super, LoxValue::Nil)),
            ("this", (TokenType::This, LoxValue::Nil)),
            ("true", (TokenType::True, LoxValue::Boolean(true))),
            ("var", (TokenType::Var, LoxValue::Nil)),
            ("while", (TokenType::While, LoxValue::Nil)),
        ] {
            let tokenizer = Tokenizer::tokenize(input, DEFAULT_TAB_SIZE as usize).unwrap();
            assert_eq!(
                extract_token_types_and_values(tokenizer.tokens),
                vec![result, (TokenType::Eof, LoxValue::Nil)]
            );
        }
    }

    #[test]
    fn test_line_comment() {
        token_type_checks(&[
            (
                "print x; // print y;",
                vec![
                    TokenType::Print,
                    TokenType::Identifier,
                    TokenType::Semicolon,
                    TokenType::Eof,
                ],
            ),
            ("( // )", vec![TokenType::LeftParen, TokenType::Eof]),
            (
                "( // )\n)",
                vec![TokenType::LeftParen, TokenType::RightParen, TokenType::Eof],
            ),
        ])
    }

    #[test]
    fn test_empty_program() {
        token_type_check("", &[TokenType::Eof])
    }

    #[test]
    fn test_idx_line_col_calculations() {
        let input = ";+,.
,==.=)
<=\t>==,  )
!=3.141\"hello

\"  \t 3.twelve\tclass世
\"bongo\"// comment!!!!
0.25}{}{";
        let tokenizer = Tokenizer::tokenize(input, DEFAULT_TAB_SIZE as usize).unwrap();
        assert_eq!(
            extract_token_positions(tokenizer.tokens),
            vec![
                (0, 1, 1),
                (1, 1, 2),
                (2, 1, 3),
                (3, 1, 4),
                (5, 2, 1),
                (6, 2, 2),
                (8, 2, 4),
                (9, 2, 5),
                (10, 2, 6),
                (12, 3, 1),
                (15, 3, 7),
                (17, 3, 9),
                (18, 3, 10),
                (21, 3, 13),
                (23, 4, 1),
                (25, 4, 3),
                (30, 4, 8),
                (43, 6, 9),
                (44, 6, 10),
                (45, 6, 11),
                (52, 6, 21),
                (61, 7, 1),
                (83, 8, 1),
                (87, 8, 5),
                (88, 8, 6),
                (89, 8, 7),
                (90, 8, 8),
                (91, 8, 9)
            ]
        )
    }

    #[test]
    fn test_lexeme_extraction() {
        let input = r#"
        // Single-character tokens
        (){}.,-+;/* 

        // One or two character tokens
        ! != = == > >= < <= 

        // Literals
        identifier "string" 123 45.67 

        // Multiline strings
        "multiline
string"
        "another
multiline
string"

        // Keywords
        and class else false for fun if nil or print return super this true var while
        "#;
        let tokenizer = Tokenizer::tokenize(input, DEFAULT_TAB_SIZE as usize).unwrap();
        assert_eq!(
            extract_token_lexemes(tokenizer.tokens),
            vec![
                "(",
                ")",
                "{",
                "}",
                ".",
                ",",
                "-",
                "+",
                ";",
                "/",
                "*",
                "!",
                "!=",
                "=",
                "==",
                ">",
                ">=",
                "<",
                "<=",
                "identifier",
                "\"string\"",
                "123",
                "45.67",
                "\"multiline\nstring\"",
                "\"another\nmultiline\nstring\"",
                "and",
                "class",
                "else",
                "false",
                "for",
                "fun",
                "if",
                "nil",
                "or",
                "print",
                "return",
                "super",
                "this",
                "true",
                "var",
                "while",
                "EOF"
            ]
        );
    }

    #[test]
    fn test_unexpected_character() {
        assert_err!(
            Tokenizer::tokenize("#", DEFAULT_TAB_SIZE as usize),
            TokenizerError::UnexpectedCharacter { .. }
        );
        assert_err!(
            Tokenizer::tokenize("^", DEFAULT_TAB_SIZE as usize),
            TokenizerError::UnexpectedCharacter { .. }
        );
        assert_err!(
            Tokenizer::tokenize("(£)", DEFAULT_TAB_SIZE as usize),
            TokenizerError::UnexpectedCharacter { .. }
        );
        assert_err!(
            Tokenizer::tokenize("'", DEFAULT_TAB_SIZE as usize),
            TokenizerError::UnexpectedCharacter { .. }
        )
    }

    #[test]
    fn test_unterminated_string_literal() {
        assert_err!(
            Tokenizer::tokenize(
                "\" hello i am an unterminated string literal!",
                DEFAULT_TAB_SIZE as usize
            ),
            TokenizerError::UnterminatedStringLiteral { .. }
        )
    }
}
