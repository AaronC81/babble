use crate::parser::Node;

use super::{ValueRef, InterpreterResult, Interpreter, InterpreterErrorKind, StackFrame, StackFrameContext, LocalVariableRef, LocalVariable};

#[derive(Debug, Clone)]
pub struct Block {
    pub body: Node,
    pub parameters: Vec<String>,
    pub captures: Vec<LocalVariableRef>,
}

// TODO: more sensible Eq implementation, maybe use some unique ID
impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}
impl Eq for Block {}

impl Block {
    pub fn arity(&self) -> usize {
        self.parameters.len()
    }

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
                    .chain(self.captures.iter().cloned())
                    .collect(),
            self_value: interpreter.current_stack_frame().self_value.clone(),
            context: StackFrameContext::Block,
        });

        // Run the body
        let result = interpreter.evaluate(&self.body)?;

        // Pop the frame
        interpreter.stack.pop();

        Ok(result)
    }
}
