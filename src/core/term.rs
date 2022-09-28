use std::rc::Rc;

use super::var;

// Dont have neither sigma types nor unit type.
// I dont want to waste my time coding these things now.
pub enum Term {
  /// Variables
  Var(var::Index),

  /// Dependent Function Type
  Pi(String, Rc<Term>, Rc<Term>),

  /// Lambda abstraction
  Lambda(String, Rc<Term>),

  /// Application
  App(Rc<Term>, Vec<Rc<Term>>),

  /// Universes / Type : Type
  Universe,
}