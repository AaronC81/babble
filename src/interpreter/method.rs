use std::{rc::Rc, fmt::Debug};

use crate::parser::Node;

use super::{Interpreter, ValueRef, InterpreterResult, InterpreterErrorKind, StackFrame, StackFrameContext, LocalVariable};

#[derive(Debug)]
pub struct Method {
    pub name: String,
    pub implementation: MethodImplementation,
}

pub type MethodRef = Rc<Method>;

impl Method {
    pub fn new_internal<F>(name: &str, function: F) -> Self
    where F: Fn(&mut Interpreter, ValueRef, Vec<ValueRef>) -> InterpreterResult + 'static {
        Self {
            name: name.into(),
            implementation: MethodImplementation::Internal(Box::new(function)),
        }
    }

    pub fn new_parsed(name: &str, body: Node, internal_names: Vec<String>) -> Self {
        Self {
            name: name.into(),
            implementation: MethodImplementation::Parsed { body, internal_names },
        }
    }
    
    pub fn rc(self) -> MethodRef {
        Rc::new(self)
    }

    pub fn arity(&self) -> usize {
        self.name.matches(':').count()
    }

    pub fn call(self: Rc<Self>, interpreter: &mut Interpreter, receiver: ValueRef, parameters: Vec<ValueRef>) -> InterpreterResult {        
        if self.arity() != parameters.len() {
            return Err(InterpreterErrorKind::IncorrectArity {
                name: self.name.clone(),
                expected: self.arity(),
                got: parameters.len(),
            }.into())
        }

        match &self.implementation {
            // Internal methods don't need a stack frame, since we control their behaviour
            MethodImplementation::Internal(func) =>
                (func)(interpreter, receiver, parameters),
            
            MethodImplementation::Parsed { body, internal_names } => {
                // Create a new stack frame with the relevant parameters
                interpreter.stack.push(StackFrame {
                    locals: internal_names.iter()
                        .cloned()
                        .zip(parameters)
                        .map(|(name, value)| LocalVariable { name, value }.rc())
                        .collect(),
                    self_value: receiver.clone(),
                    context: StackFrameContext::Method {
                        method: self.clone(),
                        receiver,
                    },
                });

                // Run the body
                let result = interpreter.evaluate(&body)?;

                // Pop the frame
                interpreter.stack.pop();

                Ok(result)
            },
        }

        // 
    }
}

pub enum MethodImplementation {
    Internal(Box<dyn Fn(&mut Interpreter, ValueRef, Vec<ValueRef>) -> InterpreterResult>),
    Parsed {
        body: Node,
        internal_names: Vec<String>,
    },
}
impl Debug for MethodImplementation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Internal(_) => f.debug_tuple("Internal").finish(),
            Self::Parsed { body, internal_names } => f.debug_struct("Parsed").field("body", body).field("internal_names", internal_names).finish(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MethodLocality {
    Instance,
    Static,
}
