use std::rc::Rc;

use crate::interpreter::value::VName;

use self::short::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RawTerm<R, N> {
    Universe,
    Type(R, R),
    Pi(R, R),
    Lam(R),
    App(R, R),
    Si(R, R),
    Pair(R, R),
    Proj(R, usize),
    Bounded(usize),
    Free(N),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Term(pub RawTerm<Rc<Term>, Rc<String>>);

pub fn app_fold(v: Vec<Rc<Term>>) -> Rc<Term> {
    v.into_iter()
        .fold(None, |res, t| match res {
            Some(term) => Some(Rc::new(Term(RawTerm::App(term, t)))),
            None => Some(t),
        })
        .unwrap()
}

pub fn proj_fold(term: Rc<Term>, v: Vec<usize>) -> Rc<Term> {
    v.into_iter()
        .fold(term, |t, p| Rc::new(Term(RawTerm::Proj(t, p))))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExtendedTerm(pub RawTerm<Rc<ExtendedTerm>, EName>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EName {
    Global(Rc<String>),
    Local(usize),
}

impl EName {
    pub fn eval(&self) -> VName {
        match self {
            EName::Global(s) => VName::Global(s.clone()),
            EName::Local(ind) => VName::Local(*ind),
        }
    }
}

impl ExtendedTerm {
    pub fn subst(&self, sub: Rc<ExtendedTerm>, ind: usize) -> Rc<ExtendedTerm> {
        match &self.0 {
            RawTerm::Universe => universe(),
            RawTerm::Type(te, ty) => typ(te.subst(sub.clone(), ind), ty.subst(sub, ind)),
            RawTerm::Pi(arg_type, res_type) => pi(
                arg_type.subst(sub.clone(), ind),
                res_type.subst(sub, ind + 1),
            ),
            RawTerm::Lam(body) => lam(body.subst(sub, ind + 1)),
            RawTerm::App(fun, arg) => app(fun.subst(sub.clone(), ind), arg.subst(sub, ind)),
            RawTerm::Si(first_type, second_type) => si(
                first_type.subst(sub.clone(), ind),
                second_type.subst(sub, ind),
            ),
            RawTerm::Pair(f, s) => pair(f.subst(sub.clone(), ind), s.subst(sub, ind)),
            RawTerm::Proj(t, p) => proj(t.subst(sub, ind), *p),
            RawTerm::Bounded(i) => {
                if *i == ind {
                    sub
                } else {
                    bounded(*i)
                }
            }
            RawTerm::Free(name) => free(name.clone()),
        }
    }
}

pub mod short {
    use super::*;

    pub(crate) fn universe() -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::Universe))
    }

    pub(crate) fn typ(te: Rc<ExtendedTerm>, ty: Rc<ExtendedTerm>) -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::Type(te, ty)))
    }

    pub(crate) fn pi(arg_type: Rc<ExtendedTerm>, res_type: Rc<ExtendedTerm>) -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::Pi(arg_type, res_type)))
    }

    pub(crate) fn lam(body: Rc<ExtendedTerm>) -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::Lam(body)))
    }

    pub(crate) fn app(fun: Rc<ExtendedTerm>, arg: Rc<ExtendedTerm>) -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::App(fun, arg)))
    }

    pub(crate) fn si(f: Rc<ExtendedTerm>, s: Rc<ExtendedTerm>) -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::Si(f, s)))
    }

    pub(crate) fn pair(f: Rc<ExtendedTerm>, s: Rc<ExtendedTerm>) -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::Pair(f, s)))
    }

    pub(crate) fn proj(t: Rc<ExtendedTerm>, p: usize) -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::Proj(t, p)))
    }

    pub(crate) fn bounded(ind: usize) -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::Bounded(ind)))
    }

    pub(crate) fn free(n: EName) -> Rc<ExtendedTerm> {
        Rc::new(ExtendedTerm(RawTerm::Free(n)))
    }
}
