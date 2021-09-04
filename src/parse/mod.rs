use std::rc::Rc;

use crate::{error::CompilerError, interior_representation::abstract_syntax_tree::Term};

pub fn parse(s: &str) -> Result<Rc<Term>, CompilerError> {
    match crate::parser::TermParser::new().parse(s) {
        Ok(ok) => Ok(ok),
        Err(err) => Err(CompilerError::ParserError(
            err.map_error(str::to_string)
                .map_token(|tok| (tok.0, tok.1.to_string())),
        )),
    }
}
