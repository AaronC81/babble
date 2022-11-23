use std::fmt::Display;

use crate::interpreter::{Value, Interpreter, InterpreterResult};

use super::{Node, NodeKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(i64),
    String(String),
    True,
    False,
    Null,
}

impl Literal {
    pub fn instantiate(&self, interpreter: &mut Interpreter) -> InterpreterResult {
        Ok(match self {
            Literal::Integer(i) => Value::new_integer(*i),
            Literal::String(s) => Value::new_string(s),
            Literal::True => Value::new_boolean(interpreter, true),
            Literal::False => Value::new_boolean(interpreter, false),
            Literal::Null => Value::new_null(),
        }.rc())
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Null => write!(f, "null"),
        }
    }
}
