use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Star,
    Type(Rc<Term>, Rc<Term>),
    Pi(Rc<Term>, Rc<Term>),
    Lam(Rc<Term>),
    App(Rc<Term>, Rc<Term>),
    Si(Rc<Term>, Rc<Term>),
    Pair(Rc<Term>, Rc<Term>),
    Proj(Rc<Term>, usize),
    Bounded(usize),
    Free(Name),
}

fn star() -> Rc<Term> {
    Rc::new(Term::Star)
}

fn typ(te: Rc<Term>, ty: Rc<Term>) -> Rc<Term> {
    Rc::new(Term::Type(te, ty))
}

fn pi(arg_type: Rc<Term>, res_type: Rc<Term>) -> Rc<Term> {
    Rc::new(Term::Pi(arg_type, res_type))
}

fn lam(body: Rc<Term>) -> Rc<Term> {
    Rc::new(Term::Lam(body))
}

fn app(fun: Rc<Term>, arg: Rc<Term>) -> Rc<Term> {
    Rc::new(Term::App(fun, arg))
}

fn si(f: Rc<Term>, s: Rc<Term>) -> Rc<Term> {
    Rc::new(Term::Si(f, s))
}

fn pair(f: Rc<Term>, s: Rc<Term>) -> Rc<Term> {
    Rc::new(Term::Pair(f, s))
}

fn proj(t: Rc<Term>, p: usize) -> Rc<Term> {
    Rc::new(Term::Proj(t, p))
}

fn bounded(ind: usize) -> Rc<Term> {
    Rc::new(Term::Bounded(ind))
}

fn free(n: Name) -> Rc<Term> {
    Rc::new(Term::Free(n))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Name {
    Global(String),
    Local(usize),
    Quote(usize),
}

pub enum List<T> {
    Cons(T, Rc<List<T>>),
    Nil,
}

impl<T> List<T> {
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
    pub fn lookupTy(&self, key: &A) -> Option<&B> {
        match self {
            List::Cons((k, v), t) => {
                if k == key {
                    Some(v)
                } else {
                    t.lookupTy(key)
                }
            }
            List::Nil => None,
        }
    }
}

pub enum Value {
    Star,
    Lam(Closure),
    Pi(Rc<Value>, Closure),
    Si(Rc<Value>, Closure),
    Pair(Rc<Value>, Rc<Value>),
    Neutral(Rc<Neutral>),
}

fn vStar() -> Rc<Value> {
    Rc::new(Value::Star)
}

fn vLam(c: Closure) -> Rc<Value> {
    Rc::new(Value::Lam(c))
}

fn vPi(arg_type: Rc<Value>, res_type: Closure) -> Rc<Value> {
    Rc::new(Value::Pi(arg_type, res_type))
}

fn vSi(first_type: Rc<Value>, second_type: Closure) -> Rc<Value> {
    Rc::new(Value::Si(first_type, second_type))
}

fn vPair(first: Rc<Value>, second: Rc<Value>) -> Rc<Value> {
    Rc::new(Value::Pair(first, second))
}

fn vNeutral(n: Rc<Neutral>) -> Rc<Value> {
    Rc::new(Value::Neutral(n))
}

pub enum Neutral {
    Free(Name),
    Pair(Rc<Neutral>, usize),
    App(Rc<Neutral>, Rc<Value>),
}

fn nFree(n: Name) -> Rc<Neutral> {
    Rc::new(Neutral::Free(n))
}

fn nPair(t: Rc<Neutral>, p: usize) -> Rc<Neutral> {
    Rc::new(Neutral::Pair(t, p))
}

fn nApp(fun: Rc<Neutral>, arg: Rc<Value>) -> Rc<Neutral> {
    Rc::new(Neutral::App(fun, arg))
}
pub struct Closure {
    pub term: Rc<Term>,
    pub env: Rc<List<Rc<Value>>>,
}

impl Closure {
    pub fn new(term: Rc<Term>, env: Rc<List<Rc<Value>>>) -> Closure {
        Closure { term, env }
    }

    pub fn apply(&self, x: Rc<Value>) -> Rc<Value> {
        self.term.eval(Rc::new(List::Cons(x, self.env.clone())))
    }
}

impl Term {
    pub fn subst(&self, sub: Rc<Term>, ind: usize) -> Rc<Term> {
        match self {
            Term::Star => star(),
            Term::Type(te, ty) => typ(te.subst(sub.clone(), ind), ty.subst(sub, ind)),
            Term::Pi(arg_type, res_type) => pi(
                arg_type.subst(sub.clone(), ind),
                res_type.subst(sub, ind + 1),
            ),
            Term::Lam(body) => lam(body.subst(sub, ind + 1)),
            Term::App(fun, arg) => app(fun.subst(sub.clone(), ind), arg.subst(sub, ind)),
            Term::Si(first_type, second_type) => si(
                first_type.subst(sub.clone(), ind),
                second_type.subst(sub, ind),
            ),
            Term::Pair(f, s) => pair(f.subst(sub.clone(), ind), s.subst(sub, ind)),
            Term::Proj(t, p) => proj(t.subst(sub, ind), *p),
            Term::Bounded(i) => {
                if *i == ind {
                    sub
                } else {
                    bounded(*i)
                }
            }
            Term::Free(name) => free(name.clone()),
        }
    }

    pub fn eval(&self, env: Rc<List<Rc<Value>>>) -> Rc<Value> {
        match self {
            Term::Type(term, _) => term.eval(env),
            Term::Star => vStar(),
            Term::Pi(arg_type, res_type) => vPi(
                arg_type.eval(env.clone()),
                Closure::new(res_type.clone(), env),
            ),
            Term::Lam(body) => vLam(Closure::new(body.clone(), env)),
            Term::App(fun, arg) => {
                let fun_val = fun.eval(env.clone());
                let arg_val = arg.eval(env);

                match fun_val.as_ref() {
                    Value::Lam(cl) => cl.apply(arg_val),
                    Value::Neutral(n) => vNeutral(nApp(n.clone(), arg_val)),
                    _ => unreachable!(),
                }
            }
            Term::Si(first_type, second_type) => vPi(
                first_type.eval(env.clone()),
                Closure::new(second_type.clone(), env),
            ),
            Term::Pair(first, second) => vPair(first.eval(env.clone()), second.eval(env)),
            Term::Proj(term, ind) => {
                let term_res = term.eval(env);
                match term_res.as_ref() {
                    Value::Pair(f, s) => {
                        if *ind == 0 {
                            f.clone()
                        } else {
                            s.clone()
                        }
                    }
                    Value::Neutral(n) => vNeutral(nPair(n.clone(), *ind)),
                    _ => unreachable!(),
                }
            }
            Term::Bounded(ind) => env.get(*ind).clone(),
            Term::Free(name) => vNeutral(nFree(name.clone())),
        }
    }
}

impl Value {
    pub fn quote(&self, ind: usize) -> Rc<Term> {
        match self {
            Value::Lam(body) => lam(body.apply(vNeutral(nFree(Name::Quote(ind)))).quote(ind + 1)),
            Value::Star => star(),
            Value::Pi(arg_type, res_type) => pi(
                arg_type.quote(ind),
                res_type
                    .apply(vNeutral(nFree(Name::Quote(ind))))
                    .quote(ind + 1),
            ),
            Value::Si(first_type, second_type) => si(
                first_type.quote(ind),
                second_type
                    .apply(vNeutral(nFree(Name::Quote(ind))))
                    .quote(ind + 1),
            ),
            Value::Pair(first, second) => pair(first.quote(ind), second.quote(ind)),
            Value::Neutral(n) => n.quote(ind),
        }
    }
}

impl Neutral {
    pub fn quote(&self, ind: usize) -> Rc<Term> {
        match self {
            Neutral::Free(name) => match name {
                Name::Quote(k) => bounded(ind - *k - 1),
                x => free(x.clone()),
            },
            Neutral::Pair(term, p) => proj(term.quote(ind), *p),
            Neutral::App(fun, arg) => app(fun.quote(ind), arg.quote(ind)),
        }
    }
}

#[derive(Clone)]
pub struct TypeContext {
    ctx: Rc<List<(Name, Rc<Value>)>>,
    n_binders: usize,
}

impl TypeContext {
    pub fn pushDecl(&self, n: Name, ty: Rc<Value>) -> TypeContext {
        TypeContext {
            ctx: Rc::new(List::Cons((n, ty), self.ctx.clone())),
            n_binders: self.n_binders,
        }
    }

    pub fn lookupTy(&self, n: &Name) -> Option<Rc<Value>> {
        self.ctx.lookupTy(n).map(|x| x.clone())
    }

    pub fn pushBinder(&self) -> TypeContext {
        TypeContext {
            ctx: self.ctx.clone(),
            n_binders: self.n_binders + 1,
        }
    }

    pub fn getFreshLN(&self) -> (Name, TypeContext) {
        (Name::Local(self.n_binders), self.pushBinder())
    }
}

impl Term {
    fn check_type(&self, ty: Rc<Value>, ctx: TypeContext) -> Result<(), ()> {
        match self {
            Term::Lam(body) => todo!(),
            Term::Pair(f, s) => todo!(),
            _ => {
                let act_type = self.typeTerm(ctx)?;
                act_type.is_eq(ty)?;
                Ok(())
            }
        }
    }

    fn typeTerm(&self, ctx: TypeContext) -> Result<Rc<Value>, ()> {
        match self {
            Term::Star => Ok(vStar()),
            Term::Type(te, ty) => {
                ty.check_type(vStar(), ctx.clone())?;
                let type_value = ty.eval(Rc::new(List::Nil));
                te.check_type(type_value.clone(), ctx)?;
                Ok(type_value)
            }
            Term::Pi(arg_type, res_type) => {
                arg_type.check_type(vStar(), ctx.clone())?;
                let arg_type_value = arg_type.eval(Rc::new(List::Nil));
                let (l_name, new_ctx) = ctx.getFreshLN();
                let res_type_subst = res_type.subst(free(l_name.clone()), 0);
                res_type_subst.check_type(vStar(), new_ctx.pushDecl(l_name, arg_type_value))?;
                Ok(vStar())
            }
            Term::Si(first_type, second_type) => {
                first_type.check_type(vStar(), ctx.clone())?;
                let first_type_value = first_type.eval(Rc::new(List::Nil));
                let (l_name, new_ctx) = ctx.getFreshLN();
                let second_type_subst = second_type.subst(free(l_name.clone()), 0);
                second_type_subst
                    .check_type(vStar(), new_ctx.pushDecl(l_name, first_type_value))?;
                Ok(vStar())
            }

            Term::App(fun, arg) => {
                let fun_type = fun.typeTerm(ctx.clone())?;
                match fun_type.as_ref() {
                    Value::Pi(arg_type, res_type) => {
                        arg.check_type(arg_type.clone(), ctx)?;
                        Ok(res_type.apply(arg.eval(Rc::new(List::Nil))))
                    }
                    _ => Err(()),
                }
            }
            Term::Proj(t, p) => {
                let t_type = t.typeTerm(ctx.clone())?;
                match t_type.as_ref() {
                    Value::Si(first_type, second_type) => match p {
                        0 => Ok(first_type.clone()),
                        1 => {
                            let t_value = proj(t.clone(), 0).eval(Rc::new(List::Nil));
                            Ok(second_type.apply(t_value))
                        }
                        _ => Err(()),
                    },
                    _ => Err(()),
                }
            }
            Term::Free(n) => match ctx.lookupTy(n) {
                Some(ty) => Ok(ty),
                None => Err(()),
            },

            Term::Lam(_) => Err(()),
            Term::Pair(_, _) => Err(()),
            Term::Bounded(_) => Err(()),
        }
    }
}

impl Value {
    fn is_eq(&self, another: Rc<Value>) -> Result<(), ()> {
        let self_type_expr = self.quote(0);
        let another_type_expr = another.quote(0);

        if self_type_expr == another_type_expr {
            Ok(())
        } else {
            Err(())
        }
    }
}
