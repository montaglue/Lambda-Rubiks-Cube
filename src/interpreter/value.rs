use std::rc::Rc;

use crate::{
    interior_representation::abstract_syntax_tree::{short::*, EName, ExtendedTerm},
    utils::list::List,
};

use self::short::*;

use super::evaluation::eval;

#[derive(Debug)]
pub enum Value {
    Universe,
    Lam(Closure),
    Pi(Rc<Value>, Closure),
    Si(Rc<Value>, Closure),
    Pair(Rc<Value>, Rc<Value>),
    Neutral(Rc<Neutral>),
}

#[derive(Debug)]
pub enum Neutral {
    Free(VName),
    Pair(Rc<Neutral>, usize),
    App(Rc<Neutral>, Rc<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VName {
    Quote(usize),
    Global(Rc<String>),
    Local(usize),
}

#[derive(Debug)]
pub struct Closure {
    pub term: Rc<ExtendedTerm>,
    pub env: Rc<List<Rc<Value>>>,
}

impl Closure {
    pub fn new(term: Rc<ExtendedTerm>, env: Rc<List<Rc<Value>>>) -> Closure {
        Closure { term, env }
    }

    pub fn apply(&self, x: Rc<Value>) -> Rc<Value> {
        eval(&self.term, Rc::new(List::Cons(x, self.env.clone())))
    }
}

impl Value {
    pub fn quote(&self, ind: usize) -> Rc<ExtendedTerm> {
        match self {
            Value::Lam(body) => lam(body
                .apply(v_neutral(n_free(VName::Quote(ind))))
                .quote(ind + 1)),
            Value::Universe => universe(),
            Value::Pi(arg_type, res_type) => pi(
                arg_type.quote(ind),
                res_type
                    .apply(v_neutral(n_free(VName::Quote(ind))))
                    .quote(ind + 1),
            ),
            Value::Si(first_type, second_type) => si(
                first_type.quote(ind),
                second_type
                    .apply(v_neutral(n_free(VName::Quote(ind))))
                    .quote(ind + 1),
            ),
            Value::Pair(first, second) => pair(first.quote(ind), second.quote(ind)),
            Value::Neutral(n) => n.quote(ind),
        }
    }
}

impl Neutral {
    pub fn quote(&self, ind: usize) -> Rc<ExtendedTerm> {
        match self {
            Neutral::Free(name) => match name {
                VName::Quote(k) => bounded(ind - *k - 1),
                VName::Global(s) => free(EName::Global(s.clone())),
                VName::Local(ind) => free(EName::Local(*ind)),
            },
            Neutral::Pair(term, p) => proj(term.quote(ind), *p),
            Neutral::App(fun, arg) => app(fun.quote(ind), arg.quote(ind)),
        }
    }
}

impl Value {
    pub fn is_eq(&self, another: Rc<Value>) -> Result<(), ()> {
        let self_type_expr = self.quote(0);
        let another_type_expr = another.quote(0);

        if self_type_expr == another_type_expr {
            Ok(())
        } else {
            Err(())
        }
    }
}

pub mod short {
    use super::*;

    pub(crate) fn v_universe() -> Rc<Value> {
        Rc::new(Value::Universe)
    }

    pub(crate) fn v_lam(c: Closure) -> Rc<Value> {
        Rc::new(Value::Lam(c))
    }

    pub(crate) fn v_pi(arg_type: Rc<Value>, res_type: Closure) -> Rc<Value> {
        Rc::new(Value::Pi(arg_type, res_type))
    }

    pub(crate) fn v_si(first_type: Rc<Value>, second_type: Closure) -> Rc<Value> {
        Rc::new(Value::Si(first_type, second_type))
    }

    pub(crate) fn v_pair(first: Rc<Value>, second: Rc<Value>) -> Rc<Value> {
        Rc::new(Value::Pair(first, second))
    }

    pub(crate) fn v_neutral(n: Rc<Neutral>) -> Rc<Value> {
        Rc::new(Value::Neutral(n))
    }

    pub(crate) fn n_free(n: VName) -> Rc<Neutral> {
        Rc::new(Neutral::Free(n))
    }

    pub(crate) fn n_pair(t: Rc<Neutral>, p: usize) -> Rc<Neutral> {
        Rc::new(Neutral::Pair(t, p))
    }

    pub(crate) fn n_app(fun: Rc<Neutral>, arg: Rc<Value>) -> Rc<Neutral> {
        Rc::new(Neutral::App(fun, arg))
    }
}
