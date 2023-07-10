//! Provides method definitions, both intrinsic and user-defined.

use std::{rc::Rc, fmt::Debug};

use crate::parser::MethodVisibility;

use super::{Interpreter, ValueRef, InterpreterResult, InterpreterErrorKind, StackFrame, StackFrameContext, LocalVariable, instruction::InstructionBlockRef, DocumentationState};

/// A named method, defined on a type for use in code.
#[derive(Debug)]
pub struct Method {
    pub name: String,
    pub implementation: MethodImplementation,
    pub documentation: DocumentationState,
    pub visibility: MethodVisibility,
    pub arity: usize,
    pub unordered: bool,
}

pub type MethodRef = Rc<Method>;

impl Method {
    /// Constructs a new method with an intrinsic definition.
    pub fn new_internal<F>(name: &str, function: F) -> Self
    where F: Fn(&mut Interpreter, ValueRef, &[ValueRef]) -> InterpreterResult + 'static {
        Self {
            name: name.into(),
            arity: Self::arity_from_name(name),
            implementation: MethodImplementation::Internal(Box::new(function)),
            documentation: DocumentationState::Undocumented,
            visibility: MethodVisibility::default(),
            unordered: false,
        }
    }

    /// Constructs a new method which executes by evaluating parsed nodes.
    pub fn new_compiled(name: &str, instructions: InstructionBlockRef, internal_names: Vec<String>) -> Self {
        Self {
            name: name.into(),
            arity: Self::arity_from_name(name),
            implementation: MethodImplementation::Compiled { instructions, internal_names },
            documentation: DocumentationState::Undocumented,
            visibility: MethodVisibility::default(),
            unordered: false,
        }
    }

    /// Constructs a new method which is implemented with interpreter magic and should never be
    /// called directly.
    pub fn new_magic(name: &str) -> Self {
        Self {
            name: name.into(),
            arity: Self::arity_from_name(name),
            implementation: MethodImplementation::Magic,
            documentation: DocumentationState::Undocumented,
            visibility: MethodVisibility::default(),
            unordered: false,
        }
    }

    /// Adds documentation to this method definition.
    pub fn add_documentation(&mut self, documentation: &str) {
        self.documentation = DocumentationState::Documented(documentation.into());
    }

    /// Consumes this method definition and returns a new one with documentation added.
    pub fn with_documentation(mut self, documentation: &str) -> Self {
        self.add_documentation(documentation);
        self
    }

    /// Consumes this method definition and returns one which is unordered.
    pub fn unordered(self) -> Self {
        Self {
            unordered: true,
            ..self
        }
    }
    
    /// Transforms this [Method] in a [MethodRef].
    pub fn rc(self) -> MethodRef {
        Rc::new(self)
    }

    /// Calculates the number of arguments expected by this method from its name.
    pub fn arity_from_name(name: &str) -> usize {
        name.matches(':').count()
    }
}

type InternalMethod = dyn Fn(&mut Interpreter, ValueRef, &[ValueRef]) -> InterpreterResult;

/// The implementation of a [Method].
pub enum MethodImplementation {
    /// The method is implemented intrinsically.
    Internal(Box<InternalMethod>),
    
    /// The method is implemented using compiled instructions, and a set of parameter names.
    Compiled {
        instructions: InstructionBlockRef,
        internal_names: Vec<String>,
    },

    /// This method uses some extreme interpreter magic - it doesn't have a body because it's
    /// handled by the interpreter itself. This is reserved only for the most special of methods,
    /// such as those which directly need to access instruction metadata.
    Magic,

    /// This method isn't actually a unique method - instead, it is a proxy for calling an unordered
    /// method.
    UnorderedProxy {
        target: MethodRef,
        argument_order: Vec<usize>,
    },
}
impl Debug for MethodImplementation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Internal(_) => f.debug_tuple("Internal").finish(),
            Self::Compiled { instructions, internal_names } => f.debug_struct("Compiled").field("instructions", instructions).field("internal_names", internal_names).finish(),
            Self::Magic => f.debug_tuple("Magic").finish(),
            Self::UnorderedProxy { argument_order, .. } => f.debug_struct("UnorderedProxy").field("argument_order", argument_order).finish(),
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
