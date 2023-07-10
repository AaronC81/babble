//! Provides an implementation of _blocks_, anonymous functions which Babble uses to implement
//! control flow.
//! 
//! See [`Block`] for more details.

use std::{sync::atomic::AtomicUsize, collections::HashMap, hash::{Hasher, Hash}};

use crate::parser::BlockParameters;

use super::{ValueRef, InterpreterResult, Interpreter, InterpreterErrorKind, StackFrame, StackFrameContext, LocalVariableRef, LocalVariable, Value, instruction::InstructionBlockRef};

static UNIQUE_BLOCK_ID: AtomicUsize = AtomicUsize::new(1);

/// An anonymous function which can take a given number of unnamed parameters and return a value.
/// 
/// Blocks act as closures by capturing local variables and `self` from the context they are created
/// in. The names of the variables which a block must capture are determined during a
/// [special analysis step after parsing](crate::parser::capture_analysis).
#[derive(Debug, Clone)]
pub struct Block {
    pub id: usize,
    pub body: InstructionBlockRef,
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

impl Hash for Block {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Block {
    pub fn new(body: InstructionBlockRef, parameters: BlockParameters, captured_locals: Vec<LocalVariableRef>, captured_self: ValueRef) -> Self {
        Self {
            id: UNIQUE_BLOCK_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            body,
            parameters,
            captured_locals,
            captured_self,
        }
    }
    
    /// The number of parameters which this block takes. If the block takes a variable number of
    /// arguments using [BlockParameters::All], this will return -1.
    pub fn arity(&self) -> isize {
        match &self.parameters {
            BlockParameters::Named(n) => n.len() as isize,
            BlockParameters::All(_) => -1,
        }
    }
}
