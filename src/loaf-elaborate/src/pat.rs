use core::fmt;
use std::{
    collections::HashSet,
    fmt::{Debug, Display, Formatter},
    rc::Rc,
};

use loaf_core::{types::Level, value::{Value, Stuck}};
use loaf_tree::{Expr, Pat};

use crate::{
    context::{Context, DeclKind},
    decls::CaseTree,
    errors::ElaborationError,
    eval::{apply, quote},
};

#[derive(Debug, Clone)]
pub enum PatTerm {
    Var(Option<String>),
    Cons(String, Level, Vec<PatTerm>),
    Absurd,
}

#[derive(Debug, Clone)]
pub struct Constraint {
    pub left: Rc<PatTerm>,
    pub right: Rc<PatTerm>,
    pub typ: Rc<Value>,
}

#[derive(Debug, Clone)]
pub struct StEquation {
    pub constraints: Vec<Constraint>,
    pub pats: Vec<PatTerm>,
}

#[derive(Debug)]
pub struct PatState {
    pub matrix: Vec<StEquation>,
    pub pats: Vec<PatTerm>,
    pub target: Rc<Value>,
}

impl PatTerm {
    fn fmt_term(&self, ctx: &Context, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            PatTerm::Var(l) => {
                write!(f, "{}", l.clone().unwrap_or("_".to_string()))
            }
            PatTerm::Cons(st, _, s) => {
                write!(f, "({}", st)?;
                for s in s {
                    write!(f, " ")?;
                    s.fmt_term(ctx, f)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            PatTerm::Absurd => {
                write!(f, "(.)")
            }
        }
    }
}

impl Constraint {
    fn fmt_term(&self, ctx: &Context, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "[")?;
        self.left.fmt_term(ctx, f)?;
        write!(f, " /? ")?;
        self.right.fmt_term(ctx, f)?;
        write!(f, " : {}", quote(ctx, self.typ.clone(), ctx.env.depth).with_ctx(&ctx.env))?;
        write!(f, "]")
    }
}

impl StEquation {
    fn fmt_term(&self, ctx: &Context, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, " |")?;
        for c in &self.constraints {
            write!(f, " ")?;
            c.fmt_term(ctx, f)?;
        }
        if self.constraints.len() > 0 {
            write!(f, " ")?;
        }
        for c in &self.pats {
            write!(f, " ")?;
            c.fmt_term(ctx, f)?;
        }
        Ok(())
    }
}

impl PatState {
    fn fmt_term(&self, ctx: &Context, f: &mut Formatter) -> Result<(), fmt::Error> {
        writeln!(f, " |- {}", quote(ctx, self.target.clone(), ctx.env.depth).with_ctx(&ctx.env))?;
        for x in &self.matrix {
            x.fmt_term(ctx, f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

struct PairSt<'a>(&'a Context, &'a PatState);

impl<'a> Display for PairSt<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.1.fmt_term(self.0, f)
    }
}

/// Elaboration of patterns

pub fn init_pat(ctx: &mut Context, counter: &mut usize, created: &mut HashSet<String>, pat: &Pat) -> Result<PatTerm, ElaborationError> {
    match pat {
        Pat::Cons { first, spine } => {
            let lvl = ctx.decls.get(&first.name).map(|x| x.clone());
            match lvl.map(|lvl| (lvl, &ctx.funs_val.get(lvl.0).unwrap().1)) {
                Some((lvl, DeclKind::DataDecl)) => {
                    let mut pats = Vec::new();
                    for pat in spine {
                        pats.push(init_pat(ctx, counter, created, pat)?);
                    }
                    Ok(PatTerm::Cons(first.name.clone(), lvl, pats))
                }
                Some(_) => Err(ElaborationError::NotADataConstructor(first.range.clone(), first.name.clone())),
                None => {
                    if spine.len() == 0 {
                        if created.contains(&first.name) {
                            Err(ElaborationError::NonLinear(first.range.clone(), first.name.clone()))
                        } else {
                            *counter = *counter + 1;
                            created.insert(first.name.clone());
                            Ok(PatTerm::Var(Some(first.name.clone())))
                        }
                    } else {
                        Err(ElaborationError::NotADataConstructor(first.range.clone(), first.name.clone()))
                    }
                }
            }
        }
        Pat::Absurd => Ok(PatTerm::Absurd),
    }
}

fn apply_pat(ctx: &mut Context, state: &mut PatState, name: &Option<String>, typ: &Rc<Value>) -> Result<(), ElaborationError> {
    let mut pats = Vec::new();
    for rule in &state.matrix {
        if rule.pats.len() != 0 {
            let mut constraints = rule.constraints.clone();
            let rule1 = rule.pats[0].clone();
            constraints.push(Constraint {
                left: Rc::new(PatTerm::Var(name.clone())),
                right: Rc::new(rule1),
                typ: typ.clone()
            });
            pats.push(StEquation {
                constraints,
                pats: rule.pats[1..].to_vec(),
            })
        }
    }
    state.matrix = pats;
    Ok(())
}

fn elaborate_pats(ctx: &mut Context, state: &mut PatState) -> Result<CaseTree, ElaborationError> {
    println!("{}", PairSt(ctx, state));
    if state.matrix.len() > 0 && state.matrix[0].constraints.len() > 0 {
        if let Value::Neutral(Stuck::Data(ind, lvl), sp) = &*state.matrix[0].constraints[0].typ {
            println!("Res {} : {:?}", ind, sp);
            todo!()
        } else {
            println!("{:?}", &*state.matrix[0].constraints[0].typ );
            todo!()
        }
    } else if let Value::Pi(name, typ, b) = &*state.target.clone() {
        let ty = Rc::new(Value::var(ctx.env.depth));
        ctx.with_ty_mut(name.clone(), typ.clone());
        state.target = apply(b, name.clone(), ty);
        apply_pat(ctx, state, name, typ)?;
        elaborate_pats(ctx, state)
    } else {
        panic!("Oh no!")
    }
}

pub fn check_pats(ctx: &mut Context, equations: &Vec<loaf_tree::Equation>, ty: Rc<Value>) -> Result<CaseTree, ElaborationError> {
    let mut matrix = Vec::new();
    for equation in equations {
        let mut counter = 0;
        let mut created = HashSet::new();
        let mut pat_terms = Vec::new();
        for pat in &equation.pats {
            pat_terms.push(init_pat(ctx, &mut counter, &mut created, &pat)?)
        }
        matrix.push(StEquation {
            constraints: Vec::new(),
            pats: pat_terms,
        });
    }
    elaborate_pats(
        ctx,
        &mut PatState {
            matrix,
            pats: Vec::new(),
            target: ty,
        },
    )
}
