use crate::parser::Node;

use super::{LexicalContextRef, ValueRef, InterpreterResult, Interpreter, InterpreterError, StackFrame, StackFrameContext};

// TODO: more sensible Eq implementation, maybe use some unique ID
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub body: Node,
    pub parameters: Vec<String>,
    pub captures: Vec<(String, ValueRef)>,
}

impl Block {
    pub fn arity(&self) -> usize {
        self.parameters.len()
    }

    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ValueRef>) -> InterpreterResult {
        if self.arity() != arguments.len() {
            return Err(InterpreterError::IncorrectArity {
                name: "anonymous block".into(),
                expected: self.arity(),
                got: arguments.len(),
            })
        }

        // Create a new stack frame with the relevant locals - that is...
        interpreter.stack.push(StackFrame {
            locals:
                // ...parameters...
                self.parameters.iter()
                    .cloned()
                    .zip(arguments)
                    // ...and captures.
                    .chain(self.captures.iter().cloned())
                    .collect(),
            context: StackFrameContext::Block,
        });

        // Run the body
        interpreter.evaluate(&self.body)
    }
}
