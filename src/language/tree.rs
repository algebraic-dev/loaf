use crate::language::position::Range;

use either::Either;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub range: Range,
}

#[derive(Debug)]
pub enum Pat {
    Cons { first: Ident, spine: Vec<Pat> },
    Absurd,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Typ {
        range: Range,
    },
    Let {
        range: Range,
        binder: Ident,
        typ: Option<Box<Expr>>,
        val: Box<Expr>,
        body: Box<Expr>,
    },
    Var {
        range: Range,
        name: Ident,
    },
    Hlp { range: Range },

    // Dependent function

    Pi {
        range: Range,
        binder: Option<Ident>,
        typ: Box<Expr>,
        body: Box<Expr>,
    },
    Lam {
        range: Range,
        binder: Ident,
        body: Box<Expr>,
    },
    App {
        range: Range,
        reason: Box<Expr>,
        spine: Vec<Expr>,
    },

    // Dependent sigma

    Sigma {
        range: Range,
        binder: Option<Ident>,
        typ: Box<Expr>,
        body: Box<Expr>,
    },
    Pair {
        range: Range,
        fst: Box<Expr>,
        snd: Box<Expr>,
    },
    Left {
        range: Range,
        expr: Box<Expr>
    },
    Right {
        range: Range,
        expr: Box<Expr>
    },

    // Other

    If {
        range: Range,
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
    },
    Ann {
        range: Range,
        expr: Box<Expr>,
        typ: Box<Expr>,
    }
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

pub trait Locate {
    fn get_range(&self) -> Range;
}

impl Locate for Expr {
    fn get_range(&self) -> Range {
        use Expr::*;
        match self {
            Typ { range } => range.clone(),
            Lam { range, .. } => range.clone(),
            Let { range, .. } => range.clone(),
            Var { range, .. } => range.clone(),
            App { range, .. } => range.clone(),
            Pi { range, .. } => range.clone(),
            If { range, .. } => range.clone(),
            Ann { range, .. } => range.clone(),
            Sigma { range, .. } => range.clone(),
            Hlp { range, .. } => range.clone(),
            Pair { range, .. } => range.clone(),
            Left { range, .. } => range.clone(),
            Right { range, .. } => range.clone(),
        }
    }
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
                if spine.is_empty() {
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
            Hlp { range : _ } => write!(f, "?"),
            Lam {
                range: _,
                binder,
                body,
            } => write!(f, "(λ {}. {})", binder, body),
            Pi {
                range: _,
                binder: Some(binder),
                typ,
                body,
            } => write!(f, "(Π {} : {} . {})", binder, typ, body),
            Ann {
                range: _,
                expr,
                typ,
            } => write!(f, "({} : {})", expr, typ),
            Pi {
                range: _,
                binder: None,
                typ,
                body,
            } => write!(f, "({} → {})", typ, body),
            Sigma {
                range: _,
                binder: None,
                typ,
                body,
            } => write!(f, "⟨{}, {}⟩", typ, body),
            Sigma {
                range: _,
                binder: Some(name),
                typ,
                body,
            } => write!(f, "(Σ {} : {}, {})", name, typ, body),
            Pair {
                range: _,
                fst,
                snd,
            } => write!(f, "⟨{}, {}⟩", fst, snd),
            Left {
                range: _,
                expr,
            } => write!(f, "(π1 {})", expr),
            Right {
                range: _,
                expr,
            } => write!(f, "(π2 {})", expr),
            Let {
                range: _,
                binder,
                typ: Some(typ),
                val,
                body,
            } => write!(f, "(let {} : {} = {} in {})", binder, typ, val, body),
            Let {
                range: _,
                binder,
                typ: None,
                val,
                body,
            } => write!(f, "(let {} = {} in {})", binder, val, body),
            Var { range: _, name } => write!(f, "{}", name),
            App {
                range: _,
                reason,
                spine,
            } => {
                if spine.is_empty() {
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
                range: _,
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
