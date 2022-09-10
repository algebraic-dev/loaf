use crate::language::position::{Point, Range};
use crate::language::tokens::Token;
use crate::language::error::SyntaxError;

use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
  input: &'a str,
  pos: Point
}

fn is_digit(chr: char) -> bool {
  chr >= '0' && chr <= '9'
}

fn is_useless(chr: char) -> bool {
  match chr {
    '\n' | '\r' | '\t' | ' ' => true,
    _ => false
  }
}

fn is_reserved(chr: char) -> bool {
  match chr {
    '(' | ')' | '.' | ':' | '|' | '=' | 'λ' | '→' | '★' | 'Π' => true,
    _ => false
  }
}

fn is_valid_id(chr: char) -> bool {
  !is_useless(chr) && !is_reserved(chr)
}

impl<'a> Lexer<'a> {

  pub fn new(input: &'a str) -> Lexer<'a> {
    Lexer {
      input,
      pos: Point::new_start()
    }
  }

  pub fn next_char(&mut self, peekable: &mut Peekable<Chars<'a>>) -> Option<char> {
    let chr = peekable.next();

    match chr {
      Some(chr) => {
        self.input = &self.input[chr.len_utf8()..];
        self.pos = self.pos.next(chr);
        Some(chr)
      }
      None => None
    }
  }

  pub fn accumulate_while(&mut self, peekable: &mut Peekable<Chars<'a>>, condition: fn(char) -> bool) -> String {
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

  pub fn single_token(&mut self, peekable: &mut Peekable<Chars<'a>>, token: Token) -> Result<(Token, Range), SyntaxError> {
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
      Some(chr) => {
        match chr {
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
          'λ' => self.single_token(&mut peekable, Token::Lambda),
          '→' => self.single_token(&mut peekable, Token::Arrow),
          '★' => self.single_token(&mut peekable, Token::Star),
          'Π' => self.single_token(&mut peekable, Token::Pi),
          '"' => {
            self.next_char(&mut peekable);
            let string = self.accumulate_while(&mut peekable, |x| x != '"');
            match peekable.peek() {
              Some('"') => {
                self.next_char(&mut peekable);
                let end = self.pos.clone();
                Ok((Token::Str(string), Range {start, end}))
              }
              _ => {
                Err(SyntaxError::UnfinishedString(start))
              }
            }
          }
          c if is_digit(*c) => {
            let number = self.accumulate_while(&mut peekable, is_digit);
            let end = self.pos.clone();
            Ok((Token::Num(number.parse::<usize>().unwrap()), Range { start, end }))
          }
          c => {
            let acc = self.accumulate_while(&mut peekable, is_valid_id);
            let end = self.pos.clone();
            match acc.as_str() {
              "let"  => Ok((Token::Let, Range { start, end })),
              "in"   => Ok((Token::In, Range { start, end })),
              "type" => Ok((Token::Type, Range { start, end })),
              _      => Ok((Token::Id(acc), Range { start, end })),
            }
          }
        }
      }
    }
  }
}

