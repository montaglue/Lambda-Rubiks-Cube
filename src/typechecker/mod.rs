pub mod type_context;

use std::rc::Rc;

use crate::{
    interior_representation::abstract_syntax_tree::{short::*, ExtendedTerm, RawTerm},
    interpreter::{
        evaluation::eval,
        value::{short::*, Value},
    },
    utils::list::List,
};

use self::type_context::TypeContext;

fn eliminate_type(term: &ExtendedTerm, ctx: TypeContext) -> Result<Rc<Value>, ()> {
    match &term.0 {
        RawTerm::Universe => Ok(v_universe()),
        RawTerm::Type(te, ty) => {
            check_type(&ty, v_universe(), ctx.clone())?;
            let type_value = eval(ty, List::new());
            check_type(&te, type_value.clone(), ctx)?;
            Ok(type_value)
        }
        RawTerm::Pi(arg_type, res_type) => {
            check_type(&arg_type, v_universe(), ctx.clone())?;
            let arg_type_value = eval(arg_type, List::new());
            let (l_name, new_ctx) = ctx.get_fresh();
            let res_type_subst = res_type.subst(free(l_name.clone()), 0);
            check_type(
                &res_type_subst,
                v_universe(),
                new_ctx.push_decl(l_name, arg_type_value),
            )?;
            Ok(v_universe())
        }
        RawTerm::Si(first_type, second_type) => {
            check_type(&first_type, v_universe(), ctx.clone())?;
            let first_type_value = eval(first_type, List::new());
            let (l_name, new_ctx) = ctx.get_fresh();
            let second_type_subst = second_type.subst(free(l_name.clone()), 0);
            check_type(
                &second_type_subst,
                v_universe(),
                new_ctx.push_decl(l_name, first_type_value),
            )?;
            Ok(v_universe())
        }
        RawTerm::App(fun, arg) => {
            let fun_type = eliminate_type(&fun, ctx.clone())?;
            match fun_type.as_ref() {
                Value::Pi(arg_type, res_type) => {
                    check_type(&arg, arg_type.clone(), ctx)?;
                    Ok(res_type.apply(eval(arg, List::new())))
                }
                _ => Err(()),
            }
        }
        RawTerm::Proj(t, p) => {
            let t_type = eliminate_type(&t, ctx.clone())?;
            match t_type.as_ref() {
                Value::Si(first_type, second_type) => match p {
                    0 => Ok(first_type.clone()),
                    1 => {
                        let t_value = eval(&proj(t.clone(), 0), List::new());
                        Ok(second_type.apply(t_value))
                    }
                    _ => Err(()),
                },
                _ => Err(()),
            }
        }
        RawTerm::Free(n) => match ctx.lookup_ty(n) {
            Some(ty) => Ok(ty),
            None => Err(()),
        },

        RawTerm::Lam(_) => Err(()),
        RawTerm::Pair(_, _) => Err(()),
        RawTerm::Bounded(_) => Err(()),
    }
}

fn check_type(term: &ExtendedTerm, ty: Rc<Value>, ctx: TypeContext) -> Result<(), ()> {
    match &term.0 {
        RawTerm::Lam(body) => todo!(),
        RawTerm::Pair(f, s) => todo!(),
        _ => {
            let act_type = eliminate_type(term, ctx)?;
            act_type.is_eq(ty)?;
            Ok(())
        }
    }
}
