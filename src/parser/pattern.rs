use std::collections::HashMap;

use crate::parser::{SendMessageComponents, SendMessageParameter, Node, NodeKind};

use crate::interpreter::{ValueRef, InterpreterError, Interpreter, Value};

use super::Literal;

#[derive(Debug, Clone)]
enum PatternParseError {
    InvalidNode(Node),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern {
    pub kind: PatternKind,
}

impl Pattern {
    fn new(kind: PatternKind) -> Self {
        Self { kind }
    }

    fn new_literal(value: Literal) -> Self {
        Self::new(PatternKind::Literal(value))
    }

    fn new_binding(name: &str) -> Self {
        Self::new(PatternKind::Binding(name.into()))
    }

    fn new_discard() -> Self {
        Self::new(PatternKind::Discard)
    }

    fn match_against(&self, value: ValueRef, context: &mut PatternMatchContext) -> Result<bool, InterpreterError> {
        match &self.kind {
            PatternKind::Literal(expected) => {
                // TODO: currently probably produces unexpected results for impure literals
                let expected_value = expected.instantiate(context.interpreter)?;
                Ok(
                    context.interpreter.send_message(
                        value,
                        &SendMessageComponents::Parameterised(vec![
                            ("equals".to_string(), SendMessageParameter::Evaluated(expected_value)),
                        ])
                    )?.borrow().to_boolean().unwrap()
                )
            },
            PatternKind::Binding(name) => {
                context.bindings.insert(name.into(), value);
                Ok(true)
            },
            PatternKind::Discard => Ok(true),
        }
    }

    fn all_bindings(&self) -> Vec<String> {
        match &self.kind {
            PatternKind::Literal(_) => vec![],
            PatternKind::Binding(name) => vec![name.clone()],
            PatternKind::Discard => vec![],
        }
    }

    fn parse(interpreter: &Interpreter, node: Node) -> Result<Self, PatternParseError> {
        match node.kind {
            // Literals
            NodeKind::Literal(l) => Ok(Pattern::new_literal(l)),

            // Compounds - all TODO currently
            NodeKind::SendMessage { receiver, components } => todo!(),
            NodeKind::EnumVariant { enum_type, variant_name, components } => todo!(),

            // Bindings
            NodeKind::Identifier(i) =>
                if i == "_" {
                    Ok(Pattern::new_discard())
                } else {
                    Ok(Pattern::new_binding(&i))
                }
            
            // Invalid
            NodeKind::StatementSequence(_)
            | NodeKind::Block { .. }
            | NodeKind::Assignment { .. }
            | NodeKind::SelfAccess
            | NodeKind::ImplBlock { .. }
            | NodeKind::FuncDefinition { .. }
            | NodeKind::EnumDefinition { .. }
            | NodeKind::StructDefinition { .. }
            | NodeKind::MixinDefinition { .. }
            | NodeKind::Use(_)
                => Err(PatternParseError::InvalidNode(node)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternKind {
    Literal(Literal),
    Binding(String),
    Discard,
}

struct PatternMatchContext<'a> {
    pub interpreter: &'a mut Interpreter,
    pub bindings: HashMap<String, ValueRef>, 
}
