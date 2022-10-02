pub mod expr;
pub mod name;

pub use expr::*;
pub use name::*;

use std::fmt;

#[derive(Debug)]
pub enum Pat {
    Cons { first: Ident, spine: Vec<Pat> },
    Absurd,
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
    pub value: Box<Expr>,
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
                if spine.is_empty() {
                    write!(f, "{}", first)
                } else {
                    write!(f, "({} {})", first, spine.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(" "))
                }
            }
            Pat::Absurd => {
                write!(f, ".")
            }
        }
    }
}

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let res = self.rules.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(" ");
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
        let res = self.constructors.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(" ");
        write!(f, "(type {} : {} {})", self.name, self.typ, res)
    }
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(let {} : {} {})", self.name, self.typ, self.value)
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
