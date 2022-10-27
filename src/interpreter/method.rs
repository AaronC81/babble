//! Provides method definitions, both intrinsic and user-defined.

use std::{rc::Rc, fmt::Debug};

use crate::parser::Node;

use super::{Interpreter, ValueRef, InterpreterResult, InterpreterErrorKind, StackFrame, StackFrameContext, LocalVariable};

/// A named method, defined on a type for use in code.
#[derive(Debug)]
pub struct Method {
    pub name: String,
    pub implementation: MethodImplementation,
    pub documentation: Option<String>,
}

pub type MethodRef = Rc<Method>;

impl Method {
    /// Constructs a new method with an intrinsic definition.
    pub fn new_internal<F>(name: &str, function: F) -> Self
    where F: Fn(&mut Interpreter, ValueRef, Vec<ValueRef>) -> InterpreterResult + 'static {
        Self {
            name: name.into(),
            implementation: MethodImplementation::Internal(Box::new(function)),
            documentation: None,
        }
    }

    /// Constructs a new method which executes by evaluating parsed nodes.
    pub fn new_parsed(name: &str, body: Node, internal_names: Vec<String>) -> Self {
        Self {
            name: name.into(),
            implementation: MethodImplementation::Parsed { body, internal_names },
            documentation: None,
        }
    }

    /// Adds documentation to this method definition.
    pub fn add_documentation(&mut self, documentation: &str) {
        self.documentation = Some(documentation.into());
    }
    
    /// Transforms this [Method] in a [MethodRef].
    pub fn rc(self) -> MethodRef {
        Rc::new(self)
    }

    /// The number of arguments expected by this method.
    pub fn arity(&self) -> usize {
        self.name.matches(':').count()
    }

    /// Calls this method, passing a receiver and a set of arguments.
    /// 
    /// If the method is implemented from parsed nodes, the method body is evaluated within a new
    /// stack frame. Intrinsic methods do not create a stack frame. Arguments are created as locals.
    /// 
    /// Returns an error if the number of arguments does not match the expected arity.
    pub fn call(self: Rc<Self>, interpreter: &mut Interpreter, receiver: ValueRef, arguments: Vec<ValueRef>) -> InterpreterResult {        
        if self.arity() != arguments.len() {
            return Err(InterpreterErrorKind::IncorrectArity {
                name: self.name.clone(),
                expected: self.arity(),
                got: arguments.len(),
            }.into())
        }

        match &self.implementation {
            // Internal methods don't need a stack frame, since we control their behaviour
            MethodImplementation::Internal(func) =>
                (func)(interpreter, receiver, arguments),
            
            MethodImplementation::Parsed { body, internal_names } => {
                // Create a new stack frame with the relevant parameters
                interpreter.stack.push(StackFrame {
                    locals: internal_names.iter()
                        .cloned()
                        .zip(arguments)
                        .map(|(name, value)| LocalVariable { name, value }.rc())
                        .collect(),
                    self_value: receiver.clone(),
                    context: StackFrameContext::Method {
                        method: self.clone(),
                        receiver,
                    },
                });

                // Run the body, bail if it fatally errored, and then pop the stack frame
                // This order may seem unintuitive - but when an error is fatal, then we want the
                // stack trace from the error to be as correct as possible, so we leave the frames
                // which errored on the stack
                let result = interpreter.evaluate(&body);
                if let Err(error) = &result && error.kind.is_fatal() {
                    return Err(error.clone());
                }
                interpreter.stack.pop();
                Ok(result?)
            },
        }
    }
}

/// The implementation of a [Method].
pub enum MethodImplementation {
    /// The method is implemented intrinsically.
    Internal(Box<dyn Fn(&mut Interpreter, ValueRef, Vec<ValueRef>) -> InterpreterResult>),
    
    /// The method is implemented using a parsed node, and a set of parameter names.
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

/// The definition location of a [Method].
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MethodLocality {
    /// The method is defined on instances of a type.
    Instance,

    /// The method is defined on a type itself, and does not need an instance to call.
    Static,
}
