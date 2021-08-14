use std::collections::VecDeque;

use crate::error::{CompilerError, Result};

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum Token {
    OpenBracket,
    CloseBracket,
    Name(String),
    StringLiteral(String),
    Number(i64),
}

impl Token {
    pub(crate) fn parse<'l>(s: &'l str) -> Token {
        match s {
            "(" => Token::OpenBracket,
            ")" => Token::CloseBracket,
            _ => {
                if let Ok(num) = s.parse::<i64>() {
                    Token::Number(num)
                } else {
                    Token::Name(String::from(s))
                }
            }
        }
    }

    pub(crate) fn complex_token(self) -> Result<String> {
        match self {
            Token::Name(s) => Ok(s),
            _ => Err(CompilerError::str(
                "Expected complex token, but found bracket",
            )),
        }
    }
}

fn parse_name_and_num(program: String) -> Vec<Token> {
    program
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(Token::parse)
        .collect()
}

pub(crate) fn tokenize(program: String) -> VecDeque<Token> {
    let mut is_string = true;
    program
        .split("'")
        .map(|s| {
            is_string = !is_string;
            if is_string {
                vec![Token::StringLiteral(String::from(s))]
            } else {
                parse_name_and_num(String::from(s))
            }
        })
        .flatten()
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::parser::tokenize::tokenize;

    #[test]
    fn test() {
        let s = tokenize(String::from("(F x (A add (T '10' Int) 1))"));
        println!("{:#?}", s);
    }
}
