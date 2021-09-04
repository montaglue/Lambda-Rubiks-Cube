use std::rc::Rc;

#[derive(Debug)]
pub enum List<T> {
    Cons(T, Rc<List<T>>),
    Nil,
}

impl<T> List<T> {
    pub fn new() -> Rc<List<T>> {
        Rc::new(List::Nil)
    }

    pub fn get(&self, ind: usize) -> &T {
        match self {
            List::Cons(h, t) => {
                if ind == 0 {
                    h
                } else {
                    t.get(ind - 1)
                }
            }
            List::Nil => unreachable!(),
        }
    }
}

impl<A: Eq, B> List<(A, B)> {
    pub fn lookup_ty(&self, key: &A) -> Option<&B> {
        match self {
            List::Cons((k, v), t) => {
                if k == key {
                    Some(v)
                } else {
                    t.lookup_ty(key)
                }
            }
            List::Nil => None,
        }
    }
}
