use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

use crate::elaborate::context::Context;

use super::var;

// Dont have neither sigma types nor unit type.
// I dont want to waste my time coding these things now.
#[derive(Debug, Clone)]
pub enum Term {
    /// Variables
    Var(var::Index),

    /// Dependent Function Type
    Pi(String, Rc<Term>, Rc<Term>),

    /// Lambda abstraction ~ Pi Introduction
    Lambda(String, Rc<Term>),

    /// Application ~ Pi Elimination
    App(Rc<Term>, Vec<Rc<Term>>),

    /// Dependent Pair type ~ Almost like existential
    Sigma(String, Rc<Term>, Rc<Term>),

    // Dependent Pair Introduction
    Pair(Rc<Term>, Rc<Term>),

    /// Dependent Pair Elimination 1
    Left(Rc<Term>),

    /// Dependent Pair Elimination 2
    Right(Rc<Term>),

    /// Let Del
    Let(String, Rc<Term>, Rc<Term>, Rc<Term>),

    /// Universes / Type : Type
    Universe,
}

pub struct PairCtx(Context, Term);

impl Term {
    pub fn pair_with(&self, ctx: Context) -> PairCtx {
        let clone = (*self).clone();
        PairCtx(ctx, clone)
    }

    pub fn print_term(
        &self,
        f: &mut Formatter,
        ctx: &im::Vector<String>,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Term::Var(var::Index(idx)) => {
                write!(
                    f,
                    "{}",
                    ctx.get(*idx as usize).unwrap_or(&format!("{}?", idx))
                )
            }
            Term::Pi(name, typ, body) => {
                if name == "_" {
                    let mut new_ctx = ctx.clone();
                    new_ctx.push_front(name.clone());
                    write!(f, "(")?;
                    typ.print_term(f, ctx)?;
                    write!(f, " -> ")?;
                    body.print_term(f, &new_ctx)?;
                    write!(f, ")")
                } else {
                    let mut new_ctx = ctx.clone();
                    new_ctx.push_front(name.clone());
                    write!(f, "(Π {} : ", name)?;
                    typ.print_term(f, ctx)?;
                    write!(f, " . ")?;
                    body.print_term(f, &new_ctx)?;
                    write!(f, ")")
                }
            }
            Term::Pair(typ, body) => {
                write!(f, "⟨")?;
                typ.print_term(f, ctx)?;
                write!(f, ", ")?;
                body.print_term(f, &ctx)?;
                write!(f, "⟩")
            }
            Term::Left(expr) => {
                write!(f, "(π1 ")?;
                expr.print_term(f, &ctx)?;
                write!(f, ")")
            },
            Term::Right(expr) => {
                write!(f, "(π2 ")?;
                expr.print_term(f, &ctx)?;
                write!(f, ")")
            },
            Term::Sigma(name, typ, body) => {
                if name == "_" {
                    let mut new_ctx = ctx.clone();
                    new_ctx.push_front(name.clone());
                    write!(f, "(Σ ")?;
                    typ.print_term(f, ctx)?;
                    write!(f, ". ")?;
                    body.print_term(f, &new_ctx)?;
                    write!(f, ")")
                } else {
                    let mut new_ctx = ctx.clone();
                    new_ctx.push_front(name.clone());
                    write!(f, "(Σ {} : ", name)?;
                    typ.print_term(f, ctx)?;
                    write!(f, ". ")?;
                    body.print_term(f, &new_ctx)?;
                    write!(f, ")")
                }
            }
            Term::Let(name, typ, val, body) => {
                let mut new_ctx = ctx.clone();
                new_ctx.push_front(name.clone());
                write!(f, "(let {} : ", name)?;
                typ.print_term(f, ctx)?;
                write!(f, " = ")?;
                val.print_term(f, ctx)?;
                write!(f, " in ")?;
                body.print_term(f, &new_ctx)?;
                write!(f, ")")
            }
            Term::Lambda(name, body) => {
                let mut new_ctx = ctx.clone();
                new_ctx.push_front(name.clone());
                write!(f, "(λ {} . ", name)?;
                body.print_term(f, &new_ctx)?;
                write!(f, ")")
            }
            Term::App(reason, spine) => {
                write!(f, "(")?;
                reason.print_term(f, ctx)?;
                for arg in spine {
                    write!(f, " ")?;
                    arg.print_term(f, ctx)?;
                }
                write!(f, ")")
            }
            Term::Universe => write!(f, "★"),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.print_term(f, &im::Vector::new())
    }
}

impl Display for PairCtx {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let names = self.0.vars.iter().map(|x| x.0.clone()).collect();
        self.1.print_term(f, &names)
    }
}
