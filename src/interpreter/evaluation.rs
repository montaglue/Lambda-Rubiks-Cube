use std::rc::Rc;

use crate::{
    interior_representation::abstract_syntax_tree::{ExtendedTerm, RawTerm},
    utils::list::List,
};

use super::value::{short::*, Closure, Value};

pub fn eval(term: &ExtendedTerm, env: Rc<List<Rc<Value>>>) -> Rc<Value> {
    match &term.0 {
        RawTerm::Type(term, _) => eval(&term, env),
        RawTerm::Universe => v_universe(),
        RawTerm::Pi(arg_type, res_type) => v_pi(
            eval(&arg_type, env.clone()),
            Closure::new(res_type.clone(), env),
        ),
        RawTerm::Lam(body) => v_lam(Closure::new(body.clone(), env)),
        RawTerm::App(fun, arg) => {
            let fun_val = eval(&fun, env.clone());
            let arg_val = eval(&arg, env);

            match fun_val.as_ref() {
                Value::Lam(cl) => cl.apply(arg_val),
                Value::Neutral(n) => v_neutral(n_app(n.clone(), arg_val)),
                _ => unreachable!(),
            }
        }
        RawTerm::Si(first_type, second_type) => v_pi(
            eval(&first_type, env.clone()),
            Closure::new(second_type.clone(), env),
        ),
        RawTerm::Pair(first, second) => v_pair(eval(&first, env.clone()), eval(&second, env)),
        RawTerm::Proj(term, ind) => {
            let term_res = eval(&term, env);
            match term_res.as_ref() {
                Value::Pair(f, s) => {
                    if *ind == 0 {
                        f.clone()
                    } else {
                        s.clone()
                    }
                }
                Value::Neutral(n) => v_neutral(n_pair(n.clone(), *ind)),
                _ => unreachable!(),
            }
        }
        RawTerm::Bounded(ind) => env.get(*ind).clone(),
        RawTerm::Free(name) => v_neutral(n_free(name.eval())),
    }
}
