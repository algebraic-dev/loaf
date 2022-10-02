use crate::errors::SyntaxError;
use crate::lexer::tokens::Token;
use crate::lexer::Lexer;

use loaf_span::Range;

#[derive(Clone)]
pub struct Parser<'cache> {
    lexer: Lexer<'cache>,
    current_token: (Token, Range),
    next_token: (Token, Range),
    after: usize,
}

impl<'cache> Parser<'cache> {
    pub fn init(file: &'cache mut str) -> Result<Parser<'cache>, SyntaxError> {
        let mut lexer = Lexer::new(file);
        let current_token = lexer.lex()?;
        let next_token = lexer.lex()?;
        Ok(Parser {
            lexer,
            current_token,
            next_token,
            after: 0,
        })
    }

    pub fn get(&self) -> &Token {
        &self.current_token.0
    }

    pub fn get_next(&self) -> &Token {
        &self.next_token.0
    }

    pub fn local_try<T>(&mut self, fun: fn(&mut Parser<'cache>) -> Result<T, SyntaxError>) -> Option<T> {
        let mut cloned = self.clone();
        match fun(&mut cloned) {
            Err(_) => None,
            Ok(res) => {
                self.lexer = cloned.lexer;
                self.current_token = cloned.current_token;
                self.next_token = cloned.next_token;
                self.after = cloned.after;
                Some(res)
            }
        }
    }

    pub fn try_single<T>(&mut self, fun: fn(&mut Parser<'cache>) -> Result<T, SyntaxError>) -> Result<Option<T>, SyntaxError> {
        let current = self.after;
        match fun(self) {
            Err(err) => {
                if current == self.after {
                    Ok(None)
                } else {
                    Err(err)
                }
            }
            Ok(res) => Ok(Some(res)),
        }
    }

    pub fn advance(&mut self) -> Result<(Token, Range), SyntaxError> {
        let cur = self.current_token.clone();
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.lex()?;
        self.after += 1;
        Ok(cur)
    }

    pub fn eat<T>(&mut self, condition: fn(&Token) -> Option<T>) -> Result<(T, Range), SyntaxError> {
        if let Some(res) = condition(&self.current_token.0) {
            let range = self.current_token.1.clone();
            self.advance()?;
            Ok((res, range))
        } else {
            Err(SyntaxError::UnexpectedToken(self.current_token.1.clone(), self.current_token.0.clone()))
        }
    }

    pub fn fail<T>(&mut self) -> Result<T, SyntaxError> {
        let (tkn, range) = self.current_token.clone();
        Err(SyntaxError::UnexpectedToken(range, tkn))
    }
}
