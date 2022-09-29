use std::{rc::Rc, fmt::{Display, Formatter}};

use super::var;

// Dont have neither sigma types nor unit type.
// I dont want to waste my time coding these things now.
#[derive(Debug)]
pub enum Term {
    /// Variables
    Var(var::Index),

    /// Dependent Function Type
    Pi(String, Rc<Term>, Rc<Term>),

    /// Let Del
    Let(String, Rc<Term>, Rc<Term>, Rc<Term>),

    /// Lambda abstraction
    Lambda(String, Rc<Term>),

    /// Application
    App(Rc<Term>, Vec<Rc<Term>>),

    /// Universes / Type : Type
    Universe,
}

impl Term {
  pub fn print_term(&self, f: &mut Formatter, ctx: &im::Vector<String>) -> Result<(), std::fmt::Error> {
    match self {
      Term::Var(var::Index(idx)) => {
        write!(f, "{}", ctx.get(*idx as usize).unwrap_or(&format!("{}?", idx)))
      }
      Term::Pi(name, typ, body) => {
        let mut new_ctx = ctx.clone();
        new_ctx.push_front(name.clone());
        write!(f, "(Π {} : ", name)?;
        typ.print_term(f, ctx)?;
        write!(f, " . ")?;
        body.print_term(f, &new_ctx)?;
        write!(f, ")")
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
        write!(f, "(λ {} : ", name)?;
        body.print_term(f, &new_ctx)?;
        write!(f, ")")
      }
      Term::App(reason, spine) => {
        write!(f, "(")?;
        reason.print_term(f, &ctx)?;
        for arg in spine {
          write!(f, " ")?;
          arg.print_term(f, &ctx)?;
        }
        write!(f, ")")
      }
      Term::Universe => write!(f, "★")
    }
  }
}

impl Display for Term {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
      self.print_term(f, &im::Vector::new())
  }
}