//! Provides an implementation of _blocks_, anonymous functions which Babble uses to implement
//! control flow.
//! 
//! See [`Block`] for more details.

use std::{sync::atomic::AtomicUsize, collections::HashMap};

use crate::parser::{Node, BlockParameters, PatternMatchContext};

use super::{ValueRef, InterpreterResult, Interpreter, InterpreterErrorKind, StackFrame, StackFrameContext, LocalVariableRef, LocalVariable, Value, instruction::InstructionBlock};

static UNIQUE_BLOCK_ID: AtomicUsize = AtomicUsize::new(1);

/// An anonymous function which can take a given number of unnamed parameters and return a value.
/// 
/// Blocks act as closures by capturing local variables and `self` from the context they are created
/// in. The names of the variables which a block must capture are determined during a
/// [special analysis step after parsing](crate::parser::capture_analysis).
#[derive(Debug, Clone)]
pub struct Block {
    pub id: usize,
    pub body: InstructionBlock,
    pub parameters: BlockParameters,
    pub captured_locals: Vec<LocalVariableRef>,
    pub captured_self: ValueRef,
}

impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for Block {}

impl Block {
    pub fn new(body: InstructionBlock, parameters: BlockParameters, captured_locals: Vec<LocalVariableRef>, captured_self: ValueRef) -> Self {
        Self {
            id: UNIQUE_BLOCK_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            body,
            parameters,
            captured_locals,
            captured_self,
        }
    }
    
    /// The number of parameters which this block takes.
    pub fn arity(&self) -> usize {
        match &self.parameters {
            BlockParameters::Named(n) => n.len(),
            BlockParameters::Patterned { patterns, .. } => patterns.len(),
        }
    }

    /// Call this block, passing it a given set of arguments, and returning the result of executing
    /// the block's body.
    /// 
    /// The block is evaluated within a new stack frame, with captured locals referenced as locals
    /// within that frame. Arguments are also created as locals.
    /// 
    /// Returns an error if the number of arguments does not match the expected arity.
    #[inline(always)]
    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ValueRef>) -> InterpreterResult {
        if self.arity() != arguments.len() {
            return Err(InterpreterErrorKind::IncorrectArity {
                name: "anonymous block".into(),
                expected: arguments.len(),
                got: self.arity(),
            }.into())
        }

        let mut wrap_result_in_match = false;
        let parameter_locals = match &self.parameters {
            // For named parameters, simply map arguments to parameters one-to-one
            BlockParameters::Named(ref parameters) => {
                parameters.iter()
                    .cloned()
                    .zip(arguments)
                    .map(|(name, value)| LocalVariable { name, value }.rc())
                    .collect::<Vec<_>>()
            },

            // For patterned parameters, match each argument against the corresponding pattern, and
            // deal with it according to the preference of the block if this fails
            BlockParameters::Patterned { patterns, fatal } => {
                let mut match_context = PatternMatchContext {
                    interpreter,
                    bindings: HashMap::new(),
                };
                if !fatal {
                    wrap_result_in_match = true;
                }
                
                for (pattern, value) in patterns.iter()
                    .cloned()
                    .zip(arguments)
                {
                    if !pattern.match_against(value.clone(), &mut match_context)? {
                        if *fatal {
                            return Err(InterpreterErrorKind::PatternMatchFailed(value, pattern).into())
                        } else {
                            return Ok(Value::new_match(interpreter, None).rc())
                        }
                    }
                }
                
                match_context.bindings.into_iter()
                    .map(|(name, value)| LocalVariable { name, value }.rc())
                    .collect::<Vec<_>>()
            }
        };

        // Create a new stack frame with the relevant locals - that is, parameters and captures
        interpreter.stack.push(StackFrame {
            locals: parameter_locals
                .into_iter()
                .chain(self.captured_locals.iter().cloned())
                .collect(),
            self_value: self.captured_self.clone(),
            context: StackFrameContext::Block,
        });

        // Run the body, bail if it fatally errored, and then pop the stack frame
        // This order may seem unintuitive - but when an error is fatal, then we want the stack
        // trace from the error to be as correct as possible, so we leave the frames which errored
        // on the stack
        let mut result = interpreter.evaluate(&self.body);
        if let Err(error) = &result && error.kind.is_fatal() {
            return Err(error.clone());
        }
        interpreter.stack.pop();

        if wrap_result_in_match {
            result = result.map(|value| Value::new_match(interpreter, Some(value)).rc());
        }

        Ok(result?)
    }
}
