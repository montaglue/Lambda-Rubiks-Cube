use std::collections::VecDeque;

use crate::error::Result;
use crate::parser::parse::remove_close;
use crate::parser::{
    parse::{get_complex, remove_open},
    tokenize::Token,
};

#[derive(Debug, Clone)]
pub(crate) enum AST {
    Constant(Atom),
    Variable(Name),
    Lambda(Box<AST>),
    Application(Box<AST>, Box<AST>),
    TypeAnotation(Box<AST>, Box<AST>),
    Function(Box<AST>, Box<AST>),
}

impl AST {
    pub fn subst(&mut self, ind: usize, replace: AST) {
        match self {
            Self::Variable(Name::Local(i)) if *i == ind => {
                *self = replace;
            }

            AST::Lambda(body) => body.subst(ind + 1, replace),

            AST::Application(fun, arg) => {
                fun.subst(ind, replace.clone());
                arg.subst(ind, replace);
            }

            AST::TypeAnotation(term, typ) => {
                term.subst(ind, replace.clone());
                typ.subst(ind, replace)
            }

            AST::Function(arg_type, result_type) => {
                arg_type.subst(ind, replace.clone());
                result_type.subst(ind + 1, replace);
            }

            _ => (),
        }
    }

    pub fn beta_eval(self) -> Self {
        match self {
            AST::Application(fun, arg) => match *fun {
                AST::Lambda(mut body) => {
                    body.subst(0, *arg);
                    body.beta_eval()
                }
                _ => AST::Application(fun, arg),
            },
            AST::Lambda(body) => AST::Lambda(Box::new(body.beta_eval())),
            AST::TypeAnotation(term, _typ) => term.beta_eval(),
            _ => self,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Atom {
    Universum,
    IntType,
    StringType,
    BooleanType,
    Int(i64),
    String(String),
    Boolean(bool),
}

impl Atom {
    pub(crate) fn is_named_atom(s: &String) -> Option<Atom> {
        if s == "Int" {
            Some(Atom::IntType)
        } else if s == "String" {
            Some(Atom::StringType)
        } else if s == "Bool" {
            Some(Atom::BooleanType)
        } else if s == "true" {
            Some(Atom::Boolean(true))
        } else if s == "false" {
            Some(Atom::Boolean(false))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Name {
    Local(usize),
    Global(String),
}
