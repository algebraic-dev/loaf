use loaf_span::{Locatable, Range};
use loaf_tree::*;

use crate::errors::SyntaxError;
use crate::lexer::tokens::Token;
use crate::macros::{eat_single, match_single};
use crate::state::Parser;

impl<'cache> Parser<'cache> {
    pub fn parse_id(&mut self) -> Result<Ident, SyntaxError> {
        let (name, range) = eat_single!(self, Token::Id(res) => res.clone())?;
        Ok(Ident::new(name, range))
    }

    fn parse_atom(&mut self) -> Result<Expr, SyntaxError> {
        match self.get() {
            Token::Id(_) => {
                let (name, range) = eat_single!(self, Token::Id(res) => res.clone())?;
                Ok(Expr::Var(Var {
                    name: Ident::new(name, range.clone()),
                    range,
                }))
            }
            Token::Hlp => Ok(Expr::Hlp(Hlp {
                range: eat_single!(self, Token::Hlp)?.1,
            })),
            Token::Star => Ok(Expr::Typ(Typ {
                range: eat_single!(self, Token::Star)?.1,
            })),
            Token::LPar => {
                self.eat(match_single!(Token::LPar))?;
                let res = self.parse_expr()?;
                self.eat(match_single!(Token::RPar))?;
                Ok(res)
            }
            _ => self.fail(),
        }
    }

    fn parse_call(&mut self) -> Result<Expr, SyntaxError> {
        let head = self.parse_atom()?;
        let mut spine = Vec::new();
        let mut last = head.locate();
        while let Some(arg) = self.try_single(|state| state.parse_atom())? {
            last = arg.locate();
            spine.push(arg);
        }
        if spine.is_empty() {
            Ok(head)
        } else {
            Ok(Expr::App(App {
                range: head.locate().mix(last),
                head: Box::new(head),
                spine,
            }))
        }
    }

    fn parse_pi(&mut self, range: Range) -> Result<Expr, SyntaxError> {
        let (name, id_range) = eat_single!(self, Token::Id(res) => res.clone())?;
        eat_single!(self, Token::Colon)?;
        let typ = self.parse_expr()?;
        eat_single!(self, Token::RPar)?;
        eat_single!(self, Token::Arrow)?;
        let body = self.parse_expr()?;
        Ok(Expr::Pi(Pi {
            range: range.mix(body.locate()),
            binder: Some(Ident::new(name, id_range)),
            typ: Box::new(typ),
            body: Box::new(body),
            implicit: false,
        }))
    }

    fn parse_arrow_partial(&mut self, start: Range, head: Expr) -> Result<Expr, SyntaxError> {
        if let Token::Arrow = self.get() {
            eat_single!(self, Token::Arrow)?;
            let body = self.parse_expr()?;
            Ok(Expr::Pi(Pi {
                range: start.mix(body.locate()),
                binder: None,
                typ: Box::new(head),
                body: Box::new(body),
                implicit: false,
            }))
        } else if let Token::Colon = self.get() {
            eat_single!(self, Token::Colon)?;
            let body = self.parse_expr()?;
            Ok(Expr::Ann(Ann {
                range: start.mix(body.locate()),
                expr: Box::new(head),
                typ: Box::new(body),
            }))
        } else {
            Ok(head)
        }
    }

    fn parse_arrow(&mut self) -> Result<Expr, SyntaxError> {
        if let Token::LPar = self.get() {
            let (_, range) = eat_single!(self, Token::LPar)?;
            if let (Token::Id(_), Token::Colon) = (self.get(), self.get_next()) {
                self.parse_pi(range)
            } else {
                let head = self.parse_expr()?;
                eat_single!(self, Token::RPar)?;
                self.parse_arrow_partial(range, head)
            }
        } else {
            let head = self.parse_call()?;
            self.parse_arrow_partial(head.locate(), head)
        }
    }

    pub fn parse_let(&mut self) -> Result<Expr, SyntaxError> {
        let (_, r) = eat_single!(self, Token::Let)?;
        let binder = self.parse_id()?;

        let typ = if let Token::Colon = self.get() {
            eat_single!(self, Token::Colon)?;
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        eat_single!(self, Token::Eq)?;
        let val = self.parse_expr()?;
        eat_single!(self, Token::In)?;
        let body = self.parse_expr()?;
        Ok(Expr::Let(Let {
            range: r.mix(body.locate()),
            binder,
            typ,
            val: Box::new(val),
            body: Box::new(body),
        }))
    }

    pub fn parse_lambda(&mut self) -> Result<Expr, SyntaxError> {
        let (_, r) = eat_single!(self, Token::Lambda)?;
        let binder = self.parse_id()?;
        eat_single!(self, Token::Dot)?;
        let body = self.parse_expr()?;
        Ok(Expr::Lam(Lam {
            range: r.mix(body.locate()),
            binder,
            body: Box::new(body),
        }))
    }

    pub fn parse_left(&mut self) -> Result<Expr, SyntaxError> {
        let (_, r) = eat_single!(self, Token::Left)?;
        let expr = self.parse_atom()?;
        Ok(Expr::Left(Left {
            range: r.mix(expr.locate()),
            expr: Box::new(expr),
        }))
    }

    pub fn parse_sigma(&mut self) -> Result<Expr, SyntaxError> {
        let (_, r) = eat_single!(self, Token::Sigma)?;
        let binder = self.parse_id()?;
        eat_single!(self, Token::Colon)?;
        let typ = self.parse_expr()?;
        eat_single!(self, Token::Dot)?;
        let body = self.parse_expr()?;
        Ok(Expr::Sigma(Sigma {
            range: r.mix(body.locate()),
            binder: Some(binder),
            typ: Box::new(typ),
            body: Box::new(body),
        }))
    }

    pub fn parse_right(&mut self) -> Result<Expr, SyntaxError> {
        let (_, r) = eat_single!(self, Token::Right)?;
        let expr = self.parse_atom()?;
        Ok(Expr::Right(Right {
            range: r.mix(expr.locate()),
            expr: Box::new(expr),
        }))
    }

    pub fn parse_pi_unicode(&mut self) -> Result<Expr, SyntaxError> {
        let (_, r) = eat_single!(self, Token::Pi)?;
        let binder = self.parse_id()?;
        eat_single!(self, Token::Colon)?;
        let typ = self.parse_expr()?;
        eat_single!(self, Token::Dot)?;
        let body = self.parse_expr()?;
        Ok(Expr::Pi(Pi {
            range: r.mix(body.locate()),
            binder: Some(binder),
            typ: Box::new(typ),
            body: Box::new(body),
            implicit: false,
        }))
    }

    pub fn parse_expr(&mut self) -> Result<Expr, SyntaxError> {
        match self.get() {
            Token::Left => self.parse_left(),
            Token::Right => self.parse_right(),
            Token::Lambda => self.parse_lambda(),
            Token::Sigma => self.parse_sigma(),
            Token::Pi => self.parse_pi_unicode(),
            Token::Let => self.parse_let(),
            _ => self.parse_arrow(),
        }
    }
}
