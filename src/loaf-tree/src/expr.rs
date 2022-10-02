use core::fmt;

use loaf_span::{Locatable, Range};

use crate::Ident;

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
    Hlp {
        range: Range,
    },

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
        expr: Box<Expr>,
    },
    Right {
        range: Range,
        expr: Box<Expr>,
    },

    Ann {
        range: Range,
        expr: Box<Expr>,
        typ: Box<Expr>,
    },
}

impl Locatable for Expr {
    fn locate(&self) -> Range {
        use Expr::*;
        match self {
            Typ { range } => range.clone(),
            Lam { range, .. } => range.clone(),
            Let { range, .. } => range.clone(),
            Var { range, .. } => range.clone(),
            App { range, .. } => range.clone(),
            Pi { range, .. } => range.clone(),
            Ann { range, .. } => range.clone(),
            Sigma { range, .. } => range.clone(),
            Hlp { range, .. } => range.clone(),
            Pair { range, .. } => range.clone(),
            Left { range, .. } => range.clone(),
            Right { range, .. } => range.clone(),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;
        match self {
            Typ { range: _ } => write!(f, "★"),
            Hlp { range: _ } => write!(f, "?"),
            Lam { range: _, binder, body } => write!(f, "(λ {}. {})", binder, body),
            Pi {
                range: _,
                binder: Some(binder),
                typ,
                body,
            } => write!(f, "(Π {} : {} . {})", binder, typ, body),
            Ann { range: _, expr, typ } => write!(f, "({} : {})", expr, typ),
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
            Pair { range: _, fst, snd } => write!(f, "⟨{}, {}⟩", fst, snd),
            Left { range: _, expr } => write!(f, "(π1 {})", expr),
            Right { range: _, expr } => write!(f, "(π2 {})", expr),
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
            App { range: _, reason, spine } => {
                if spine.is_empty() {
                    write!(f, "{}", reason)
                } else {
                    write!(f, "({} {})", reason, spine.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(" "))
                }
            }
        }
    }
}
