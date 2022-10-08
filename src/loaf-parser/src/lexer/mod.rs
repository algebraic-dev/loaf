pub mod tokens;

use crate::errors::SyntaxError;
use crate::lexer::tokens::Token;
use loaf_span::{Position, Range};

use std::iter::Peekable;
use std::str::Chars;

pub use tokens::*;

#[derive(Clone)]
pub struct Lexer<'cache> {
    input: &'cache str,
    pos: Position,
}

fn is_digit(chr: char) -> bool {
    ('0'..='9').contains(&chr)
}

fn is_useless(chr: char) -> bool {
    matches!(chr, '\n' | '\r' | '\t' | ' ')
}

fn is_reserved(chr: char) -> bool {
    matches!(chr, '(' | ')' | '.' | ':' | '|' | '=' | 'λ' | '→' | '★' | 'Π' | ',' | 'Σ')
}

fn is_valid_id(chr: char) -> bool {
    !is_reserved(chr) && !is_useless(chr)
}

impl<'cache> Lexer<'cache> {
    pub fn new(input: &'cache str) -> Lexer<'cache> {
        Lexer {
            input,
            pos: Position::new_start(),
        }
    }

    pub fn next_char(&mut self, peekable: &mut Peekable<Chars<'cache>>) -> Option<char> {
        let chr = peekable.next();

        match chr {
            Some(chr) => {
                self.input = &self.input[chr.len_utf8()..];
                self.pos = self.pos.next(chr);
                Some(chr)
            }
            None => None,
        }
    }

    pub fn accumulate_while(&mut self, peekable: &mut Peekable<Chars<'cache>>, condition: fn(char) -> bool) -> String {
        let mut str = String::new();

        while let Some(&x) = peekable.peek() {
            if condition(x) {
                str.push(x);
                self.next_char(peekable);
            } else {
                break;
            }
        }

        str
    }

    pub fn lex_string(&mut self, start: Position, peekable: &mut Peekable<Chars<'cache>>) -> Result<(Token, Range), SyntaxError> {
        self.next_char(peekable);
        let string = self.accumulate_while(peekable, |x| x != '"');
        match peekable.peek() {
            Some('"') => {
                self.next_char(peekable);
                let end = self.pos.clone();
                Ok((Token::Str(string), Range { start, end }))
            }
            _ => Err(SyntaxError::UnfinishedString(start)),
        }
    }

    pub fn lex_number(&mut self, start: Position, peekable: &mut Peekable<Chars<'cache>>) -> Result<(Token, Range), SyntaxError> {
        let number = self.accumulate_while(peekable, is_digit);
        let end = self.pos.clone();
        Ok((Token::Num(number.parse::<usize>().unwrap()), Range { start, end }))
    }

    pub fn lex_id(&mut self, start: Position, peekable: &mut Peekable<Chars<'cache>>) -> Result<(Token, Range), SyntaxError> {
        let acc = self.accumulate_while(peekable, is_valid_id);
        let end = self.pos.clone();
        match acc.as_str() {
            "let" => Ok((Token::Let, Range { start, end })),
            "in" => Ok((Token::In, Range { start, end })),
            "type" => Ok((Token::Type, Range { start, end })),
            "if" => Ok((Token::If, Range { start, end })),
            "then" => Ok((Token::Then, Range { start, end })),
            "else" => Ok((Token::Else, Range { start, end })),
            "fun" => Ok((Token::Lambda, Range { start, end })),

            // To avoid unicode
            "->" => Ok((Token::Arrow, Range { start, end })),
            "Type" => Ok((Token::Star, Range { start, end })),
            "proj1" => Ok((Token::Left, Range { start, end })),
            "proj2" => Ok((Token::Right, Range { start, end })),
            _ => Ok((Token::Id(acc), Range { start, end })),
        }
    }

    pub fn single_token(&mut self, peekable: &mut Peekable<Chars<'cache>>, token: Token) -> Result<(Token, Range), SyntaxError> {
        let start = self.pos.clone();
        self.next_char(peekable);
        let end = self.pos.clone();
        Ok((token, Range { start, end }))
    }

    pub fn lex(&mut self) -> Result<(Token, Range), SyntaxError> {
        let mut peekable = self.input.chars().peekable();
        let start = self.pos.clone();
        match peekable.peek() {
            None => Ok((Token::Eof, Range { start: start.clone(), end: start })),
            Some(chr) => match chr {
                c if is_useless(*c) => {
                    self.accumulate_while(&mut peekable, is_useless);
                    self.lex()
                }
                '(' => self.single_token(&mut peekable, Token::LPar),
                ')' => self.single_token(&mut peekable, Token::RPar),
                '.' => self.single_token(&mut peekable, Token::Dot),
                ':' => self.single_token(&mut peekable, Token::Colon),
                '|' => self.single_token(&mut peekable, Token::Pipe),
                '=' => self.single_token(&mut peekable, Token::Eq),
                ',' => self.single_token(&mut peekable, Token::Comma),
                'λ' => self.single_token(&mut peekable, Token::Lambda),
                '→' => self.single_token(&mut peekable, Token::Arrow),
                '★' => self.single_token(&mut peekable, Token::Star),
                'Π' => self.single_token(&mut peekable, Token::Pi),
                'Σ' => self.single_token(&mut peekable, Token::Sigma),
                '?' => self.single_token(&mut peekable, Token::Hlp),
                '_' => self.single_token(&mut peekable, Token::Hole),
                '"' => self.lex_string(start, &mut peekable),
                c if is_digit(*c) => self.lex_number(start, &mut peekable),
                c if is_valid_id(*c) => self.lex_id(start, &mut peekable),
                c => Err(SyntaxError::UnexpectedChar(self.pos.clone(), *c)),
            },
        }
    }
}
