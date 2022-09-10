use crate::language::position::Range;

use either::Either;
use std::fmt;

#[derive(Debug)]
pub struct Ident {
    pub name: String,
    pub range: Range,
}

#[derive(Debug)]
pub enum Pat {
    Cons { first: Ident, spine: Vec<Pat> },
    Absurd,
}

#[derive(Debug)]
pub enum Expr {
    Typ {
        range: Range,
    },
    Lam {
        binder: Ident,
        body: Box<Expr>,
    },
    Pi {
        binder: Option<Ident>,
        typ: Box<Expr>,
        body: Box<Expr>,
    },
    Let {
        binder: Ident,
        typ: Option<Box<Expr>>,
        val: Box<Expr>,
        body: Box<Expr>,
    },
    Var {
        name: Ident,
    },
    App {
        reason: Box<Expr>,
        spine: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct Equation {
    pub rules: Vec<Pat>,
    pub value: Box<Expr>,
}

#[derive(Debug)]
pub struct Constructor {
    pub name: Ident,
    pub typ: Box<Expr>,
}

#[derive(Debug)]
pub struct Decl {
    pub name: Ident,
    pub typ: Box<Expr>,
    pub value: Either<Box<Expr>, Vec<Equation>>,
}

#[derive(Debug)]
pub struct TypeDecl {
    pub name: Ident,
    pub typ: Box<Expr>,
    pub constructors: Vec<Constructor>,
}

#[derive(Debug)]
pub enum TopLevel {
    TypeDecl(TypeDecl),
    LetDecl(Decl),
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pat::Cons { first, spine } => {
                if spine.len() == 0 {
                    write!(f, "{}", first)
                } else {
                    write!(
                        f,
                        "({} {})",
                        first,
                        spine
                            .iter()
                            .map(|x| format!("{}", x))
                            .collect::<Vec<String>>()
                            .join(" ")
                    )
                }
            }
            Pat::Absurd => {
                write!(f, ".")
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;
        match self {
            Typ { range: _ } => write!(f, "★"),
            Lam { binder, body } => write!(f, "(λ {}. {})", binder, body),
            Pi {
                binder: Some(binder),
                typ,
                body,
            } => write!(f, "(Π {} : {} . {})", binder, typ, body),
            Pi {
                binder: None,
                typ,
                body,
            } => write!(f, "({} → {})", typ, body),
            Let {
                binder,
                typ: Some(typ),
                val,
                body,
            } => write!(f, "(let {} : {} = {} in {})", binder, typ, val, body),
            Let {
                binder,
                typ: None,
                val,
                body,
            } => write!(f, "(let {} = {} in {})", binder, val, body),
            Var { name } => write!(f, "{}", name),
            App { reason, spine } => {
                if spine.len() == 0 {
                    write!(f, "{}", reason)
                } else {
                    write!(
                        f,
                        "({} {})",
                        reason,
                        spine
                            .iter()
                            .map(|x| format!("{}", x))
                            .collect::<Vec<String>>()
                            .join(" ")
                    )
                }
            }
            If {
                cond: _,
                then: _,
                els: _,
            } => {
                write!(f, "Todo!")
            }
        }
    }
}

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let res = self
            .rules
            .iter()
            .map(|x| format!("{}", x))
            .collect::<Vec<String>>()
            .join(" ");
        write!(f, "(| {} := {})", res, self.value)
    }
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(| {} : {})", self.name, self.typ)
    }
}

impl fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let res = self
            .constructors
            .iter()
            .map(|x| format!("{}", x))
            .collect::<Vec<String>>()
            .join(" ");
        write!(f, "(type {} : {} {})", self.name, self.typ, res)
    }
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(let {} : {} {})",
            self.name,
            self.typ,
            match &self.value {
                Either::Left(r) => format!("{}", r),
                Either::Right(eqs) => eqs
                    .iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join(" "),
            }
        )
    }
}

impl fmt::Display for TopLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TopLevel::TypeDecl(x) => write!(f, "{}", x),
            TopLevel::LetDecl(x) => write!(f, "{}", x),
        }
    }
}
