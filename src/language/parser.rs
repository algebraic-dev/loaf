use crate::language::error::SyntaxError;
use crate::language::lexer::Lexer;
use crate::language::position::Range;
use crate::language::tokens::Token;
use crate::language::tree::*;

use either::Either;

macro_rules! match_single {
    ($pattern:pat) => {
        |x| match x {
            $pattern => Some(()),
            _ => None,
        }
    };

    ($pattern:pat => $then:expr) => {
        |x| match x {
            $pattern => Some($then),
            _ => None,
        }
    };
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: (Token, Range),
    next_token: (Token, Range),
}

impl<'a> Parser<'a> {
    pub fn init(str: &'a mut str) -> Result<Parser<'a>, SyntaxError> {
        let mut lexer = Lexer::new(str);
        let current_token = lexer.lex()?;
        let next_token = lexer.lex()?;
        Ok(Parser {
            lexer,
            current_token,
            next_token,
        })
    }

    fn get(&self) -> &Token {
        &self.current_token.0
    }

    fn advance(&mut self) -> Result<(Token, Range), SyntaxError> {
        let cur = self.current_token.clone();
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.lex()?;
        Ok(cur)
    }

    fn eat<T>(&mut self, condition: fn(&Token) -> Option<T>) -> Result<(T, Range), SyntaxError> {
        let (tkn, range) = self.advance()?;
        if let Some(res) = condition(&tkn) {
            Ok((res, range))
        } else {
            Err(SyntaxError::Unexpected(tkn, range))
        }
    }

    pub fn fail<T>(&mut self) -> Result<T, SyntaxError> {
        let (tkn, range) = self.current_token.clone();
        Err(SyntaxError::Unexpected(tkn, range))
    }

    // Parse patterns

    pub fn parse_id(&mut self) -> Result<Ident, SyntaxError> {
        let (name, range) = self.eat(|x| match x {
            Token::Id(res) => Some(res.clone()),
            _ => None,
        })?;
        Ok(Ident {
            name: name.to_string(),
            range,
        })
    }

    pub fn parse_pat(&mut self) -> Result<Pat, SyntaxError> {
        match self.get() {
            Token::LPar => {
                self.eat(match_single!(Token::LPar))?;
                let res = self.parse_pat()?;
                self.eat(match_single!(Token::RPar))?;
                Ok(res)
            }
            Token::Dot => {
                self.eat(match_single!(Token::Dot))?;
                Ok(Pat::Absurd)
            }
            Token::Id(_) => {
                let first = self.parse_id()?;
                let mut spine = Vec::new();
                while let Token::Id(_) | Token::LPar = self.get() {
                    spine.push(self.parse_pat()?);
                }
                Ok(Pat::Cons { first, spine })
            }
            _ => self.fail(),
        }
    }

    // Parse Expr.

    pub fn parse_atom(&mut self) -> Result<Expr, SyntaxError> {
        match self.get() {
            Token::Id(_) => {
                let name = self.parse_id()?;
                Ok(Expr::Var { name })
            }
            Token::Star => {
                let (_, range) = self.eat(match_single!(Token::Star))?;
                Ok(Expr::Typ { range })
            }
            Token::LPar => {
                self.eat(match_single!(Token::LPar))?;
                let res = self.parse_expr()?;
                self.eat(match_single!(Token::RPar))?;
                Ok(res)
            }
            _ => self.fail(),
        }
    }

    pub fn parse_call(&mut self) -> Result<Expr, SyntaxError> {
        let reason = self.parse_atom()?;
        let mut spine = Vec::new();
        while let Token::Id(_) | Token::LPar | Token::Then | Token::Else = self.get() {
            spine.push(self.parse_atom()?);
        }
        Ok(Expr::App {
            reason: Box::new(reason),
            spine,
        })
    }

    pub fn parse_arrow(&mut self) -> Result<Expr, SyntaxError> {
        let typ = self.parse_call()?;
        if let Token::Arrow = self.get() {
            self.eat(match_single!(Token::Arrow))?;
            let body = self.parse_arrow()?;
            Ok(Expr::Pi {
                binder: None,
                typ: Box::new(typ),
                body: Box::new(body),
            })
        } else {
            Ok(typ)
        }
    }

    pub fn parse_expr(&mut self) -> Result<Expr, SyntaxError> {
        match self.get() {
            Token::Lambda => {
                self.eat(match_single!(Token::Lambda))?;
                let binder = self.parse_id()?;
                self.eat(match_single!(Token::Dot))?;
                let body = self.parse_expr()?;
                Ok(Expr::Lam {
                    binder,
                    body: Box::new(body),
                })
            }
            Token::Pi => {
                self.eat(match_single!(Token::Pi))?;
                let binder = self.parse_id()?;
                self.eat(match_single!(Token::Colon))?;
                let typ = self.parse_expr()?;
                self.eat(match_single!(Token::Dot))?;
                let body = self.parse_expr()?;
                Ok(Expr::Pi {
                    binder: Some(binder),
                    typ: Box::new(typ),
                    body: Box::new(body),
                })
            }
            Token::Let => {
                self.eat(match_single!(Token::Let))?;
                let binder = self.parse_id()?;

                let typ = if let Token::Colon = self.get() {
                    self.eat(match_single!(Token::Colon))?;
                    Some(Box::new(self.parse_expr()?))
                } else {
                    None
                };

                self.eat(match_single!(Token::Eq))?;
                let val = self.parse_expr()?;
                self.eat(match_single!(Token::In))?;
                let body = self.parse_expr()?;
                Ok(Expr::Let {
                    binder,
                    typ,
                    val: Box::new(val),
                    body: Box::new(body),
                })
            }
            Token::If => {
                self.eat(match_single!(Token::If))?;
                let cond = Box::new(self.parse_expr()?);
                self.eat(match_single!(Token::Then))?;
                let then = Box::new(self.parse_expr()?);
                self.eat(match_single!(Token::Else))?;
                let els = Box::new(self.parse_expr()?);
                Ok(Expr::If { cond, then, els })
            }
            _ => self.parse_arrow(),
        }
    }

    // Parse top level

    pub fn parse_equation(&mut self) -> Result<Equation, SyntaxError> {
        self.eat(match_single!(Token::Pipe))?;
        let mut rules = vec![self.parse_pat()?];
        while let Token::Comma = self.get() {
            self.eat(match_single!(Token::Comma))?;
            rules.push(self.parse_pat()?);
        }
        self.eat(match_single!(Token::Eq))?;
        let body = self.parse_expr()?;
        Ok(Equation {
            rules,
            value: Box::new(body),
        })
    }

    pub fn parse_decl(&mut self) -> Result<Decl, SyntaxError> {
        self.eat(match_single!(Token::Let))?;
        let name = self.parse_id()?;
        self.eat(match_single!(Token::Colon))?;
        let typ = Box::new(self.parse_expr()?);
        if let Token::Eq = self.get() {
            self.eat(match_single!(Token::Eq))?;
            let res = self.parse_expr()?;
            Ok(Decl {
                name,
                typ,
                value: Either::Left(Box::new(res)),
            })
        } else {
            let mut equations = vec![self.parse_equation()?];
            while let Token::Pipe = self.get() {
                equations.push(self.parse_equation()?);
            }
            Ok(Decl {
                name,
                typ,
                value: Either::Right(equations),
            })
        }
    }

    pub fn parse_constructor(&mut self) -> Result<Constructor, SyntaxError> {
        self.eat(match_single!(Token::Pipe))?;
        let name = self.parse_id()?;
        self.eat(match_single!(Token::Colon))?;
        let typ = self.parse_expr()?;
        Ok(Constructor {
            name,
            typ: Box::new(typ),
        })
    }

    pub fn parse_type_decl(&mut self) -> Result<TypeDecl, SyntaxError> {
        self.eat(match_single!(Token::Type))?;
        let name = self.parse_id()?;
        self.eat(match_single!(Token::Colon))?;
        let typ = Box::new(self.parse_expr()?);
        let mut constructors = vec![self.parse_constructor()?];
        while let Token::Pipe = self.get() {
            constructors.push(self.parse_constructor()?);
        }
        Ok(TypeDecl {
            name,
            typ,
            constructors,
        })
    }

    pub fn parse_program(&mut self) -> Result<Vec<TopLevel>, SyntaxError> {
        let mut program = Vec::new();
        loop {
            match self.get() {
                Token::Let => program.push(TopLevel::LetDecl(self.parse_decl()?)),
                Token::Type => program.push(TopLevel::TypeDecl(self.parse_type_decl()?)),
                Token::Eof => break,
                _ => self.fail()?,
            }
        }
        Ok(program)
    }
}
