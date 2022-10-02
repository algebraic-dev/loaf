use core::fmt;

use loaf_span::{Locatable, Range};

use crate::Ident;

#[derive(Debug, Clone)]
pub struct Typ {
    pub range: Range,
}
#[derive(Debug, Clone)]
pub struct Let {
    pub range: Range,
    pub binder: Ident,
    pub typ: Option<Box<Expr>>,
    pub val: Box<Expr>,
    pub body: Box<Expr>,
}
#[derive(Debug, Clone)]
pub struct Var {
    pub range: Range,
    pub name: Ident,
}
#[derive(Debug, Clone)]
pub struct Hlp {
    pub range: Range,
}

// Dependent function
#[derive(Debug, Clone)]
pub struct Pi {
    pub range: Range,
    pub binder: Option<Ident>,
    pub typ: Box<Expr>,
    pub body: Box<Expr>,
}
#[derive(Debug, Clone)]
pub struct Lam {
    pub range: Range,
    pub binder: Ident,
    pub body: Box<Expr>,
}
#[derive(Debug, Clone)]
pub struct App {
    pub range: Range,
    pub head: Box<Expr>,
    pub spine: Vec<Expr>,
}

// Dependent sigma
#[derive(Debug, Clone)]
pub struct Sigma {
    pub range: Range,
    pub binder: Option<Ident>,
    pub typ: Box<Expr>,
    pub body: Box<Expr>,
}
#[derive(Debug, Clone)]
pub struct Pair {
    pub range: Range,
    pub fst: Box<Expr>,
    pub snd: Box<Expr>,
}
#[derive(Debug, Clone)]
pub struct Left {
    pub range: Range,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Right {
    pub range: Range,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Ann {
    pub range: Range,
    pub expr: Box<Expr>,
    pub typ: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Typ(Typ),
    Let(Let),
    Var(Var),
    Hlp(Hlp),
    Pi(Pi),
    Lam(Lam),
    App(App),
    Sigma(Sigma),
    Pair(Pair),
    Left(Left),
    Right(Right),
    Ann(Ann),
}

impl Locatable for Expr {
    fn locate(&self) -> Range {
        match self {
            Expr::Typ(Typ { range }) => range.clone(),
            Expr::Lam(Lam { range, .. }) => range.clone(),
            Expr::Let(Let { range, .. }) => range.clone(),
            Expr::Var(Var { range, .. }) => range.clone(),
            Expr::App(App { range, .. }) => range.clone(),
            Expr::Pi(Pi { range, .. }) => range.clone(),
            Expr::Ann(Ann { range, .. }) => range.clone(),
            Expr::Sigma(Sigma { range, .. }) => range.clone(),
            Expr::Hlp(Hlp { range, .. }) => range.clone(),
            Expr::Pair(Pair { range, .. }) => range.clone(),
            Expr::Left(Left { range, .. }) => range.clone(),
            Expr::Right(Right { range, .. }) => range.clone(),
        }
    }
}

impl fmt::Display for Typ {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "★")
    }
}

impl fmt::Display for Hlp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "?")
    }
}

impl fmt::Display for Lam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(λ {}. {})", self.binder, self.body)
    }
}

impl fmt::Display for Pi {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.binder {
            Some(binder) => write!(f, "(Π {} : {} . {})", binder, self.typ, self.body),
            None => write!(f, "({} -> {})", self.typ, self.body),
        }
    }
}

impl fmt::Display for Ann {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} : {})", self.expr, self.typ)
    }
}

impl fmt::Display for Sigma {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.binder {
            Some(binder) => write!(f, "(Σ {} : {} . {})", binder, self.typ, self.body),
            None => write!(f, "(Σ {} . {})", self.typ, self.body),
        }
    }
}

impl fmt::Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "⟨{}, {}⟩", self.fst, self.snd)
    }
}

impl fmt::Display for Left {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(π1 {})", self.expr)
    }
}

impl fmt::Display for Right {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(π2 {})", self.expr)
    }
}

impl fmt::Display for Let {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.typ {
            Some(x) => write!(f, "(let {} : {} = {} in {})", self.binder, x, self.val, self.body),
            None => write!(f, "(let {} = {} in {})", self.binder, self.val, self.body),
        }
    }
}

impl fmt::Display for App {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.spine.is_empty() {
            write!(f, "{}", self.head)
        } else {
            write!(f, "({} {})", self.head, self.spine.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(" "))
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;
        match self {
            Typ(x) => write!(f, "{}", x),
            Hlp(x) => write!(f, "{}", x),
            Lam(x) => write!(f, "{}", x),
            Pi(x) => write!(f, "{}", x),
            Ann(x) => write!(f, "{}", x),
            Sigma(x) => write!(f, "{}", x),
            Pair(x) => write!(f, "{}", x),
            Left(x) => write!(f, "{}", x),
            Right(x) => write!(f, "{}", x),
            Let(x) => write!(f, "{}", x),
            Var(x) => write!(f, "{}", x),
            App(x) => write!(f, "{}", x),
        }
    }
}
