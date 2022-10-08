pub mod errors;
pub mod expr;
pub mod lexer;
pub mod state;

#[macro_use]
pub mod macros;

use errors::SyntaxError;
pub use expr::*;
use lexer::tokens::Token;
use loaf_tree::{Constructor, Decl, DeclRes, Equation, Pat, TopLevel, TypeDecl};
pub use state::*;

impl<'cache> Parser<'cache> {
    fn parse_pat(&mut self) -> Result<Pat, SyntaxError> {
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

    fn parse_equation(&mut self) -> Result<Equation, SyntaxError> {
        self.eat(match_single!(Token::Pipe))?;
        let mut pats = vec![self.parse_pat()?];
        while let Token::Comma = self.get() {
            self.eat(match_single!(Token::Comma))?;
            pats.push(self.parse_pat()?);
        }
        self.eat(match_single!(Token::Eq))?;
        let body = self.parse_expr()?;
        Ok(Equation { pats, value: Box::new(body) })
    }

    fn parse_constructor(&mut self) -> Result<Constructor, SyntaxError> {
        self.eat(match_single!(Token::Pipe))?;
        let name = self.parse_id()?;
        self.eat(match_single!(Token::Colon))?;
        let typ = self.parse_expr()?;
        Ok(Constructor { name, typ: Box::new(typ) })
    }

    fn parse_type_decl(&mut self) -> Result<TypeDecl, SyntaxError> {
        self.eat(match_single!(Token::Type))?;
        let name = self.parse_id()?;
        self.eat(match_single!(Token::Colon))?;
        let typ = Box::new(self.parse_expr()?);
        let mut constructors = vec![self.parse_constructor()?];
        while let Token::Pipe = self.get() {
            constructors.push(self.parse_constructor()?);
        }
        Ok(TypeDecl { name, typ, constructors })
    }

    fn parse_decl(&mut self) -> Result<Decl, SyntaxError> {
        let _ = self.eat(match_single!(Token::Let))?;
        let name = self.parse_id()?;
        self.eat(match_single!(Token::Colon))?;
        let typ = Box::new(self.parse_expr()?);
        if *self.get() == Token::Pipe {
            let mut rules = Vec::new();
            while *self.get() == Token::Pipe {
                rules.push(self.parse_equation()?);
            }
            Ok(Decl {
                name,
                typ,
                value: DeclRes::Pattern(rules),
            })
        } else {
            self.eat(match_single!(Token::Eq))?;
            let res = self.parse_expr()?;
            Ok(Decl {
                name,
                typ,
                value: DeclRes::Value(Box::new(res)),
            })
        }
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
        eat_single!(self, Token::Eof)?;
        Ok(program)
    }
}
