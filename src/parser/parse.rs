use std::{
    collections::{HashMap, VecDeque},
    env::var,
};

use crate::{
    error::{CompilerError, Result},
    interior_representation::abstract_syntax_tree::{Atom, Name, AST},
};

use super::tokenize::Token;

pub(crate) fn parse(
    tokens: &mut VecDeque<Token>,
    vareables: &mut HashMap<String, usize>,
) -> Result<Box<AST>> {
    let first = get_first(tokens)?;

    let res = Ok(Box::new(match first {
        Token::OpenBracket => {
            let ind = get_complex(tokens)?;

            let res = match &ind[..] {
                "L" => {
                    let name = get_complex(tokens)?;
                    vareables.insert(name.clone(), vareables.len() + 1);
                    let body = parse(tokens, vareables)?;
                    vareables.remove(&name);

                    AST::Lambda(body)
                }

                "F" => {
                    let name = get_complex(tokens)?;
                    let arg_type = parse(tokens, vareables)?;
                    vareables.insert(name.clone(), vareables.len() + 1);
                    let result_type = parse(tokens, vareables)?;
                    vareables.remove(&name);

                    AST::Function(arg_type, result_type)
                }

                "A" => {
                    let fun = parse(tokens, vareables)?;
                    let arg = parse(tokens, vareables)?;

                    AST::Application(fun, arg)
                }

                "T" => {
                    let term = parse(tokens, vareables)?;
                    let typ = parse(tokens, vareables)?;

                    AST::TypeAnotation(term, typ)
                }

                _ => {
                    return Err(CompilerError::new(String::from(
                        "unknown identification token",
                    )))
                }
            };

            remove_close(tokens)?;
            res
        }

        Token::Name(s) => {
            if let Some(atom) = Atom::is_named_atom(&s) {
                AST::Constant(atom)
            } else {
                AST::Variable(if let Some(num) = vareables.get(&s) {
                    let num = vareables.len() - *num;
                    Name::Local(num)
                } else {
                    Name::Global(s)
                })
            }
        }

        Token::StringLiteral(s) => AST::Constant(Atom::String(s)),

        Token::Number(n) => AST::Constant(Atom::Int(n)),

        _ => return Err(CompilerError::new(String::from("unexpected tocken"))),
    }));
    res
}

pub(crate) fn get_first(tokens: &mut VecDeque<Token>) -> Result<Token> {
    tokens
        .pop_front()
        .ok_or(CompilerError::str("Unexpected end of tokens"))
}

pub(crate) fn get_complex(tokens: &mut VecDeque<Token>) -> Result<String> {
    get_first(tokens)?.complex_token()
}

pub(crate) fn remove_open(tokens: &mut VecDeque<Token>) -> Result<()> {
    if Token::OpenBracket == get_first(tokens)? {
        Ok(())
    } else {
        Err(CompilerError::str("Expected open bracket, but not found"))
    }
}

pub(crate) fn remove_close(tokens: &mut VecDeque<Token>) -> Result<()> {
    if Token::CloseBracket == get_first(tokens)? {
        Ok(())
    } else {
        Err(CompilerError::str("Expected close bracket, but not found"))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::interior_representation::abstract_syntax_tree::{Atom, AST};
    use crate::parser::{parse::parse, tokenize::tokenize};

    #[test]
    fn test() {
        let mut t = tokenize(String::from("(L x (A (A add x) 1))"));

        println!("{:#?}", t);

        let mut s = parse(&mut t, &mut HashMap::new()).unwrap();

        println!("{:#?}", s);

        s.subst(0, AST::Constant(Atom::Int(30)));

        println!("{:#?}", s);
    }
}
