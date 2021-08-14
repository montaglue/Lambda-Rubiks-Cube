#[derive(Debug)]
pub struct CompilerError {
    text: String,
}

impl CompilerError {
    pub fn new(text: String) -> CompilerError {
        CompilerError { text }
    }

    pub fn str<'l>(text: &'l str) -> CompilerError {
        CompilerError {
            text: String::from(text),
        }
    }
}

pub type Result<T> = std::result::Result<T, CompilerError>;
