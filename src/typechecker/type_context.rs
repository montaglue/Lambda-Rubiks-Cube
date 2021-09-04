use std::rc::Rc;

use crate::{
    interior_representation::abstract_syntax_tree::EName, interpreter::value::Value,
    utils::list::List,
};

#[derive(Clone)]
pub struct TypeContext {
    ctx: Rc<List<(EName, Rc<Value>)>>,
    n_binders: usize,
}

impl TypeContext {
    pub fn push_decl(&self, n: EName, ty: Rc<Value>) -> TypeContext {
        TypeContext {
            ctx: Rc::new(List::Cons((n, ty), self.ctx.clone())),
            n_binders: self.n_binders,
        }
    }

    pub fn lookup_ty(&self, n: &EName) -> Option<Rc<Value>> {
        self.ctx.lookup_ty(n).map(|x| x.clone())
    }

    pub fn push_binder(&self) -> TypeContext {
        TypeContext {
            ctx: self.ctx.clone(),
            n_binders: self.n_binders + 1,
        }
    }

    pub fn get_fresh(&self) -> (EName, TypeContext) {
        (EName::Local(self.n_binders), self.push_binder())
    }
}
