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

            NodeKind::StringLiteral(s) => Ok(Value::new_string(s).rc()),

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

            NodeKind::Block { body, parameters, captures } => {
                // Grab captures from local variables
                let capture_values = captures
                    .iter()
                    .map(|name|
                        self.find_local(name)
                            .ok_or(InterpreterError::MissingCaptureName(name.clone()))
                            .map(|v| (name.clone(), v))
                    )
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Value::new_block(Block {
                    body: *body.clone(),
                    parameters: parameters.clone(),
                    captures: capture_values,
                }).rc())
            },

            NodeKind::EnumVariant { enum_type, variant_name, components } => {
                // TODO: enums with fields untested

                // Resolve the type and its variants
                let enum_type =
                    if let TypeInstance::Type(t) = &self.evaluate(enum_type)?.borrow().type_instance {
                        t.clone()
                    } else {
                        // Should never parse
                        unreachable!()
                    };

                // Find details of the requested variant
                let (variant_idx, variant) = enum_type.resolve_variant(&variant_name)?;

                // Check that names of passed fields match expected fields
                let given_field_names = match components {
                    SendMessageComponents::Unary(_) => vec![],
                    SendMessageComponents::Parameterised(params) => params.iter().map(|(n, _)| n.clone()).collect()
                };
                if given_field_names != variant.fields {
                    return Err(InterpreterError::IncorrectVariantParameters)
                }

                // Evaluate fields
                let field_values = match components {
                    crate::parser::SendMessageComponents::Unary(_) => vec![],
                    crate::parser::SendMessageComponents::Parameterised(params) =>
                        params.iter().map(|(_, p)| self.evaluate(p)).collect::<Result<Vec<_>, _>>()?,
                };

                Ok(Value {
                    type_instance: TypeInstance::Fields {
                        source_type: enum_type,
                        variant: Some(variant_idx),
                        field_values,
                    }
                }.rc())
            },

            NodeKind::TrueLiteral => Ok(Value::new_boolean(self, true).rc()),
            NodeKind::FalseLiteral => Ok(Value::new_boolean(self, false).rc()),
            NodeKind::NullLiteral => Ok(Value::new_null().rc()),
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

    pub fn current_stack_frame(&self) -> &StackFrame {
        self.stack.last().unwrap()
    }

    pub fn current_stack_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.last_mut().unwrap()
    }

    pub fn find_local(&self, name: &str) -> Option<ValueRef> {
        self.current_stack_frame()
            .locals
            .iter()
            .find_map(|(n, v)| if n == &name { Some(v.clone()) } else { None })
    }

    pub fn create_local(&mut self, name: &str, value: ValueRef) {
        self.current_stack_frame_mut().locals.push((name.into(), value))
    }
}
