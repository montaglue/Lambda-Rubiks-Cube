use std::rc::Rc;

use lalrpop_util::ParseError;

use crate::interpreter::value::Value;

#[derive(Debug)]
pub enum CompilerError {
    ParserError(ParseError<usize, (usize, String), String>),
    EvaluationError,
    TypingError(Rc<Value>, Rc<Value>),
    Undefined,
}
