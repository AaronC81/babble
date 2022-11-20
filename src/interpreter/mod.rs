//! Implements the main Babble interpreter.
//! 
//! Babble does not use bytecode - instead, nodes are executed using their parse tree and some 
//! surrounding context. This is pretty slow, but works well enough!

use std::{fmt::Display, rc::Rc, cell::RefCell};

use crate::{parser::{NodeKind, Node, SendMessageComponents, Parser}, tokenizer::Tokenizer, source::SourceFile};

mod error;

pub use error::*;

mod value;
pub use value::*;

mod r#type;
pub use r#type::*;

mod block;
pub use block::*;

mod method;
pub use method::*;

use self::instruction::{compile, InstructionBlock};

pub mod stdlib;
pub mod mixin_derive;

pub mod instruction;

#[cfg(test)]
pub mod tests;

/// A named local variable with a value.
#[derive(Debug)]
pub struct LocalVariable {
    name: String,
    value: ValueRef,
}

impl LocalVariable {
    /// Transforms this into a [LocalVariableRef].
    pub fn rc(self) -> LocalVariableRef {
        Rc::new(RefCell::new(self))
    }
}

pub type LocalVariableRef = Rc<RefCell<LocalVariable>>;

/// A stack frame, which provides a value for `self`, and may hold the definition (or captures, in
/// the case of a block) of a set of local variables.
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub context: StackFrameContext,
    self_value: ValueRef,
    locals: Vec<LocalVariableRef>,
}

/// A description of the context which created a stack frame.
#[derive(Debug, Clone)]
pub enum StackFrameContext {
    /// The stack frame is the root of the main source file. 
    Root,

    /// The stack frame is an `impl` block on a type.
    Impl(TypeRef),

    /// The stack frame is a call to a [`Method`].
    Method {
        method: MethodRef,
        receiver: ValueRef,
    },

    /// The stack frame is a call to a [`Block`].
    Block,
}

impl Display for StackFrameContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackFrameContext::Root =>
                f.write_str("root"),
            StackFrameContext::Impl(t) =>
                write!(f, "impl block on `{}`", (&**t).borrow().id),
            StackFrameContext::Method { method, receiver } =>
                write!(f, "method `{}` on `{}`", method.name, (&**receiver).borrow().to_language_string() ),
            StackFrameContext::Block =>
                f.write_str("block"),
        }
    }
}

/// The main interpreter context, which holds the current execution stack, and a repository of all
/// defined types.
pub struct Interpreter {
    pub types: Vec<TypeRef>,
    pub stack: Vec<StackFrame>,
}

pub type InterpreterResult = Result<ValueRef, InterpreterError>;

impl Interpreter {
    /// Creates a new interpreter, with a stack containing only a root frame, and an instance of
    /// the standard library in its type repository.
    pub fn new() -> Result<Self, InterpreterError> {
        let mut result = Self {
            types: vec![],
            stack: vec![
                StackFrame {
                    context: StackFrameContext::Root,
                    self_value: Value::new_null().rc(),
                    locals: vec![],
                }
            ],
        };
        stdlib::instantiate(&mut result)?;
        Ok(result)
    }

    /// Tokenize, parse, and compile a source file, **panicking** if this fails, and then evaluate
    /// it within this interpreter.
    pub fn parse_and_evaluate(&mut self, source_file: Rc<SourceFile>) -> InterpreterResult {
        let tokens = Tokenizer::tokenize(source_file.clone()).expect("tokenization failed");
        let node = Parser::parse_and_analyse(source_file.clone(), &tokens[..]).expect("parsing failed");
        let compiled = compile(node).expect("compilation failed");
        self.evaluate(&compiled)
    }

    /// Evaluate a single instruction block within this interpreter.
    pub fn evaluate(&mut self, instructions: &InstructionBlock) -> InterpreterResult {
        todo!();
    }

    /// Retrieve a type by name, or return `None` if it does not exist.
    pub fn resolve_type(&self, id: &str) -> Option<TypeRef> {
        self.types.iter().find(|t| t.borrow().id == id).cloned()
    }

    /// A stricter strategy for resolving types, which instead panics if the type does not exist.
    /// 
    /// Should only be used for types which are guaranteed to exist under normal circumstances, such
    /// as `Boolean`.
    pub fn resolve_stdlib_type(&self, id: &str) -> TypeRef {
        self.resolve_type(id).unwrap_or_else(|| panic!("internal error: stdlib type {} missing", id))
    }

    /// Sends a message (i.e. calls a method), given a receiver, method name, and a set of values as
    /// arguments.
    /// 
    /// Returns an error if the method does not exist on the receiver.
    pub fn send_message(&mut self, receiver: ValueRef, method_name: &str, args: Vec<ValueRef>) -> InterpreterResult {
        let receiver_ref = receiver.borrow();

        let method =
            if let TypeInstance::Type(t) = &receiver_ref.type_instance {
                t.borrow().resolve_static_method(&method_name)
            } else {
                receiver_ref.type_instance.get_type(self).borrow().resolve_instance_method(&method_name)
            };
        if let Some(method) = method {
            drop(receiver_ref);

            // Call the method
            // (This creates a frame if necessary)
            method.call(self, receiver, args)
        } else {
            Err(InterpreterErrorKind::MissingMethod(receiver.clone(), method_name.into()).into())
        }
    }

    /// Returns a reference to the current top-most stack frame.
    pub fn current_stack_frame(&self) -> &StackFrame {
        self.stack.last().unwrap()
    }

    /// Returns a mutable reference to the current top-most stack frame.
    pub fn current_stack_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.last_mut().unwrap()
    }

    /// Finds a local variable within the current stack frame, and returns a reference to it if it
    /// exists, or `None` if it doesn't.
    /// 
    /// Babble's only additional form of scoping within methods are blocks, which will capture
    /// locals when necessary and bring them into the current frame's locals. As such, this does not
    /// recurse into deeper frames than the current top-most one.
    pub fn find_local(&self, name: &str) -> Option<LocalVariableRef> {
        self.current_stack_frame()
            .locals
            .iter()
            .find(|l| l.borrow().name == name)
            .cloned()
    }

    /// Creates a new local variable on the current top-most stack frame.
    pub fn create_local(&mut self, name: &str, value: ValueRef) {
        self.current_stack_frame_mut().locals.push(LocalVariable {
            name: name.into(),
            value,
        }.rc())
    }
}
