use std::{cell::RefCell, rc::Rc};

use crate::parser::{NodeKind, Node, SendMessageComponents};

mod error;

pub use error::*;

mod value;
pub use value::*;

mod r#type;
pub use r#type::*;

mod block;
pub use block::*;

mod tests;

pub mod stdlib;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexicalContext {
    parent: Option<LexicalContextRef>,
}

impl LexicalContext {
    pub fn new_top_level() -> Self {
        LexicalContext { parent: None }
    }

    pub fn new_with_parent(parent: LexicalContextRef) -> Self {
        LexicalContext { parent: Some(parent) }
    }

    pub fn rc(self) -> LexicalContextRef {
        Rc::new(RefCell::new(self))
    }
}

pub type LexicalContextRef = Rc<RefCell<LexicalContext>>;

pub struct StackFrame {
    context: StackFrameContext,
    locals: Vec<(String, ValueRef)>,
}

pub enum StackFrameContext {
    Root,
    InternalMethod(InternalMethodRef),
    Block,
}

pub struct Interpreter {
    types: Vec<Rc<Type>>,
    stack: Vec<StackFrame>,
}

pub type InterpreterResult = Result<ValueRef, InterpreterError>;

impl Interpreter {
    pub fn new() -> Self {
        Self {
            types: stdlib::types(),
            stack: vec![
                StackFrame {
                    context: StackFrameContext::Root,
                    locals: vec![],
                }
            ],
        }
    }

    pub fn evaluate(&mut self, node: &Node) -> InterpreterResult {
        match &node.kind {
            NodeKind::IntegerLiteral(i) => (*i).try_into()
                .map(|i| Value::new_integer(i).rc())
                .map_err(|_| InterpreterError::IntegerOverflow(node.location)),

            NodeKind::SendMessage { receiver, components } => {
                // Evaluate the receiver
                let receiver = self.evaluate(&receiver)?;

                self.send_message(receiver, components)
            },

            NodeKind::StatementSequence(seq) => {
                // Bodies return their last statement, except if they have no statements, in which
                // case they return null
                let mut result = Value::new_null().rc();
                for node in seq {
                    result = self.evaluate(node)?;
                }
                Ok(result)
            },

            NodeKind::Assignment { target, value } => {
                // Only supported assignment target currently is a plain identifier
                if let box Node { kind: NodeKind::Identifier(id), .. } = target {
                    let value = self.evaluate(value)?;
                    let mut context = node.context.borrow_mut();

                    if let Some(target) = self.find_local(&id) {
                        *target.borrow_mut() = value.borrow().clone();
                    } else {
                        self.create_local(&id, value.clone());
                    }

                    Ok(value)
                } else {
                    Err(InterpreterError::InvalidAssignmentTarget(target.location))
                }
            },

            NodeKind::Identifier(id) => {
                if let Some(value) = self.find_local(id) {
                    Ok(value)
                } else if let Some(t) = self.resolve_type(id) {
                    Ok(Value::new_type(t.clone()).rc())
                } else {
                    Err(InterpreterError::MissingName(id.into(), node.location))
                }
            },

            NodeKind::Block { body, parameters } => {
                Ok(Value::new_block(Block {
                    body: *body.clone(),
                    parameters: parameters.clone(),
                }).rc())
            },
        }
    }

    pub fn resolve_type(&self, id: &str) -> Option<Rc<Type>> {
        self.types.iter().find(|t| &t.id == id).cloned()
    }

    pub fn resolve_stdlib_type(&self, id: &str) -> Rc<Type> {
        self.resolve_type(id).expect(&format!("internal error: stdlib type {} missing", id))
    }

    pub fn send_message(&mut self, receiver: ValueRef, components: &SendMessageComponents) -> InterpreterResult {
        let receiver_ref = receiver.borrow();

        let method_name = components.to_method_name();
        let method =
            if let TypeInstance::Type(t) = &receiver_ref.type_instance {
                t.resolve_static_method(&method_name)
            } else {
                receiver_ref.type_instance.get_type(&self).resolve_method(&method_name)
            };
        if let Some(method) = method {
            drop(receiver_ref);

            // Evaluate parameters
            let parameters = match components {
                crate::parser::SendMessageComponents::Unary(_) => vec![],
                crate::parser::SendMessageComponents::Parameterised(params) =>
                    params.iter().map(|(_, p)| self.evaluate(p)).collect::<Result<Vec<_>, _>>()?,
            };

            // Create a new stack frame, call the method within it, and pop the frame
            self.stack.push(StackFrame {
                context: StackFrameContext::InternalMethod(method.clone()),
                locals: vec![],
            });
            let result = method.call(self, receiver, parameters);
            self.stack.pop();

            result
        } else {
            Err(InterpreterError::MissingMethod(method_name))
        }
    }

    pub fn find_local(&self, name: &str) -> Option<ValueRef> {
        self.stack.iter()
            .rev()
            .find_map(|frame|
                frame.locals.iter().find_map(|(n, v)| if n == &name { Some(v.clone()) } else { None })
            )
    }

    pub fn create_local(&mut self, name: &str, value: ValueRef) {
        self.stack.last_mut().unwrap().locals.push((name.into(), value))
    }
}
