use crate::interpreter::{Value, Interpreter, InterpreterResult};

use super::{Node, NodeKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(i64),
    String(String),
    Array(Vec<Node>),
    True,
    False,
    Null,
}

impl Literal {
    pub fn is_pure(&self) -> bool {
        match self {
            Literal::Array(nodes) => nodes.iter()
                .all(|node| if let NodeKind::Literal(l) = &node.kind {
                    l.is_pure()
                } else {
                    false
                }),
            
            _ => true,
        }
    }

    pub fn instantiate(&self, interpreter: &mut Interpreter) -> InterpreterResult {
        Ok(match self {
            Literal::Integer(i) => Value::new_integer(*i),
            Literal::String(s) => Value::new_string(s),
            Literal::Array(nodes) => Value::new_array(&nodes.iter()
                .map(|node| interpreter.evaluate(node))
                .collect::<Result<Vec<_>, _>>()?),
            Literal::True => Value::new_boolean(interpreter, true),
            Literal::False => Value::new_boolean(interpreter, false),
            Literal::Null => Value::new_null(),
        }.rc())
    }
}
