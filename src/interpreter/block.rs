//! Provides an implementation of _blocks_, anonymous functions which Babble uses to implement
//! control flow.
//! 
//! See [`Block`] for more details.

use crate::parser::Node;

use super::{ValueRef, InterpreterResult, Interpreter, InterpreterErrorKind, StackFrame, StackFrameContext, LocalVariableRef, LocalVariable};

/// An anonymous function which can take a given number of unnamed parameters and return a value.
/// 
/// Blocks act as closures by capturing local variables and `self` from the context they are created
/// in. The names of the variables which a block must capture are determined during a
/// [special analysis step after parsing](crate::parser::capture_analysis).
#[derive(Debug, Clone)]
pub struct Block {
    pub body: Node,
    pub parameters: Vec<String>,
    pub captured_locals: Vec<LocalVariableRef>,
    pub captured_self: ValueRef,
}

// TODO: more sensible Eq implementation, maybe use some unique ID
impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}
impl Eq for Block {}

impl Block {
    /// The number of parameters which this block takes.
    pub fn arity(&self) -> usize {
        self.parameters.len()
    }

    /// Call this block, passing it a given set of arguments, and returning the result of executing
    /// the block's body.
    /// 
    /// The block is evaluated within a new stack frame, with captured locals referenced as locals
    /// within that frame. Arguments are also created as locals.
    /// 
    /// Returns an error if the number of arguments does not match the expected arity.
    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ValueRef>) -> InterpreterResult {
        if self.arity() != arguments.len() {
            return Err(InterpreterErrorKind::IncorrectArity {
                name: "anonymous block".into(),
                expected: arguments.len(),
                got: self.arity(),
            }.into())
        }

        // Create a new stack frame with the relevant locals - that is...
        interpreter.stack.push(StackFrame {
            locals:
                // ...parameters...
                self.parameters.iter()
                    .cloned()
                    .zip(arguments)
                    .map(|(name, value)| LocalVariable { name, value }.rc())
                    // ...and captures.
                    .chain(self.captured_locals.iter().cloned())
                    .collect(),
            self_value: self.captured_self.clone(),
            context: StackFrameContext::Block,
        });

        // Run the body, bail if it fatally errored, and then pop the stack frame
        // This order may seem unintuitive - but when an error is fatal, then we want the stack
        // trace from the error to be as correct as possible, so we leave the frames which errored
        // on the stack
        let result = interpreter.evaluate(&self.body);
        if let Err(error) = &result && error.kind.is_fatal() {
            return Err(error.clone());
        }
        interpreter.stack.pop();
        Ok(result?)
    }
}
