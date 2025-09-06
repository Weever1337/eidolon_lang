use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(f64),
    Identifier(String),
    GlobalIdentifier(String),
    String(String),
    Let,
    If,
    Else,
    Fun,
    Sum,
    In,
    DotDotDot,
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    GreaterThan,
    LessThan,
    EqualsEquals,
    Equals,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            chars: source.char_indices().peekable(),
            pos: 0,
        }
    }

    fn read_while<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some((_, c)) = self.chars.peek() {
            if predicate(*c) {
                self.pos += c.len_utf8();
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Token {
        self.read_while(|c| c.is_whitespace());

        let start_pos = self.pos;

        let Some((_, c)) = self.chars.next() else {
            return Token { kind: TokenKind::Eof, pos: start_pos };
        };
        self.pos += c.len_utf8();

        let kind = match c {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '^' => TokenKind::Caret,
            ',' => TokenKind::Comma,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '>' => {
                if let Some((_, '=')) = self.chars.peek() {
                    self.chars.next();
                    self.pos += 1;
                    TokenKind::GreaterThanOrEqual
                } else {
                    TokenKind::GreaterThan
                }
            }
            '<' => {
                if let Some((_, '=')) = self.chars.peek() {
                    self.chars.next();
                    self.pos += 1;
                    TokenKind::LessThanOrEqual
                } else {
                    TokenKind::LessThan
                }
            }
            '"' => {
                let start = self.pos;
                self.read_while(|c| c != '"');
                let content = &self.source[start..self.pos];
                if let Some((_, '"')) = self.chars.peek() {
                    self.chars.next();
                    self.pos += 1;
                } else {
                    panic!("Unterminated string literal");
                }
                TokenKind::String(content.to_string())
            }
            '.' => {
                if let Some((_, '.')) = self.chars.peek() {
                    self.chars.next();
                    self.pos += 1;
                    if let Some((_, '.')) = self.chars.peek() {
                        self.chars.next();
                        self.pos += 1;
                        TokenKind::DotDotDot
                    } else {
                        panic!("Expected '...'")
                    }
                } else {
                    panic!("Unexpected character: .")
                }
            }
            '=' => {
                if let Some((_, '=')) = self.chars.peek() {
                    self.chars.next();
                    self.pos += 1;
                    TokenKind::EqualsEquals
                } else {
                    TokenKind::Equals
                }
            }
            '#' => {
                self.read_while(|c| c != '\n');
                return self.next_token();
            }
            '$' => {
                let start = self.pos;
                self.read_while(|c| c.is_alphanumeric() || c == '_');
                let ident = &self.source[start..self.pos];
                TokenKind::GlobalIdentifier(ident.to_string())
            }
            c if c.is_ascii_digit() => {
                let start = start_pos;
                self.read_while(|c| c.is_ascii_digit());

                if self.chars.peek().map_or(false, |&(_, c)| c == '.') {
                    let mut peek_ahead = self.chars.clone();
                    peek_ahead.next();
                    if peek_ahead.peek().map_or(false, |&(_, c)| c.is_ascii_digit()) {
                        let (_, dot) = self.chars.next().unwrap();
                        self.pos += dot.len_utf8();
                        self.read_while(|c| c.is_ascii_digit());
                    }
                }

                let num_str = &self.source[start..self.pos];
                TokenKind::Number(num_str.parse().expect("Invalid number format"))
            }
            c if c.is_alphabetic() || c == '_' => {
                self.read_while(|c| c.is_alphanumeric() || c == '_');
                let ident_str = &self.source[start_pos..self.pos];
                match ident_str {
                    "let" => TokenKind::Let,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "fun" => TokenKind::Fun,
                    "sum" => TokenKind::Sum,
                    "in" => TokenKind::In,
                    _ => TokenKind::Identifier(ident_str.to_string()),
                }
            }
            _ => panic!("Unexpected character: {}", c),
        };

        Token { kind, pos: start_pos }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.kind == TokenKind::Eof {
            None
        } else {
            Some(token)
        }
    }
}