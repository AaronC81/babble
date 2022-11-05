//! Models errors which can occur during code execution.

use std::fmt::Display;

use crate::{source::Location, parser::{Node, Pattern}};

use super::{ValueRef, StackFrame, Interpreter};

/// A fatal error encountered while the interpreter evaluated a node.
/// 
/// Errors can optionally contain additional details, such as a location and stack backtrace, but
/// the [`Interpreter`] is rarely available in the contexts where an error is created. As such, 
/// these details are injected by the interpreter if a node evaluates to an error.
#[derive(Debug, Clone)]
pub struct InterpreterError {
    pub kind: InterpreterErrorKind,
    pub details: Option<InterpreterErrorDetails>,
}

impl InterpreterError {
    /// Adds details to this error using the given interpreter, if the error has no details 
    /// currently.
    /// 
    /// If it already has details, returns the error unmodified.
    pub fn add_details(self, node: &Node, interpreter: &Interpreter) -> Self {
        if self.details.is_some() {
            self
        } else {
            Self {
                details: Some(InterpreterErrorDetails {
                    location: Some(node.location.clone()),
                    backtrace: interpreter.stack.clone(),
                }),
                ..self
            }
        }
    }
}

/// Additional details about an error - see [InterpreterError].
#[derive(Debug, Clone)]
pub struct InterpreterErrorDetails {
    pub location: Option<Location>,
    pub backtrace: Vec<StackFrame>,
}

/// The core cause of an [InterpreterError].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpreterErrorKind {
    /// The receiver of a call did not have a method to satisfy the call.
    MissingMethod(ValueRef, String),

    /// An identifier did not exist, for example as a type or local variable.
    MissingName(String),

    /// A variable referenced by an explicit block capture did not exist.
    MissingCaptureName(String),

    /// An enum variant constructor used a variant which did not exist.
    MissingVariant(String, String),

    /// Integer arithmetic produced an integer which is too large to represent.
    IntegerOverflow,

    /// A method was passed the incorrect number of arguments.
    /// 
    /// This is unlikely to occur in normal interpreter usage, as the method naming scheme means
    /// that naming and arity are intertwined, so a [`InterpreterErrorKind::MissingMethod`] error
    /// would occur instead.
    IncorrectArity {
        name: String,
        expected: usize,
        got: usize,
    },

    /// A block was passed the incorrect number of arguments.
    IncorrectBlockArity {
        expected: usize,
        got: usize,
    },

    /// An argument to an internal method received an argument of the wrong type.
    IncorrectType, // TODO more details

    /// An expression was used as on the left-hand side of an assignment which does not represent a
    /// value which can be assigned to, such as a local variable or field.
    InvalidAssignmentTarget,

    /// An enum variant constructor was used on a value which isn't an enum.
    VariantAccessOnNonEnum,

    /// An enum variant constructor received an incorrect set of parameters.
    IncorrectVariantParameters,

    /// A function definition was not valid here.
    FuncDefinitionInvalidContext,

    /// A type with the given name has already been defined elsewhere.
    DuplicateTypeDefinition(String),

    /// A mixin `use` was not valid here.
    UseInvalidContext,

    /// A `use` statement was invoked with something that isn't a mixin.
    UseNonMixin(String),

    /// In a `![ ... ]` block, a passed argument did not match the corresponding pattern.
    PatternMatchFailed(ValueRef, Pattern),

    /// A test within the language standard library failed.
    /// 
    /// Should never occur in normal usage.
    InternalTestFailed(String),

    /// Code executed by the interpreter called `Program error: "something"`.
    ProgramError(String),

    /// Not really an error - used by `Program throw: ...` to unwind the stack
    Throw(ValueRef),
}

impl InterpreterErrorKind {
    pub fn is_fatal(&self) -> bool {
        match self {
            InterpreterErrorKind::Throw(_) => false,
            _ => true,
        }
    }
}

impl Into<InterpreterError> for InterpreterErrorKind {
    fn into(self) -> InterpreterError {
        InterpreterError {
            kind: self,
            details: None,
        }
    }
}

impl Display for InterpreterErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterErrorKind::MissingMethod(val, m) =>
                write!(f, "`{}` has no method `{}`", val.borrow().to_language_string(), m),
            InterpreterErrorKind::MissingName(name) => 
                write!(f, "`{}` does not exist", name),
            InterpreterErrorKind::MissingCaptureName(name) =>
                write!(f, "`{}` cannot be captured because it does not exist", name),
            InterpreterErrorKind::MissingVariant(type_id, var) =>
                write!(f, "enum `{}` has no variant `{}`", type_id, var),
            InterpreterErrorKind::IntegerOverflow =>
                write!(f, "integer overflow"),

            InterpreterErrorKind::IncorrectArity { name, expected, got } =>
                write!(f, "method `{}` expects {} arguments, but got {}", name, expected, got),
            InterpreterErrorKind::IncorrectBlockArity { expected, got } =>
                write!(f, "block expected {} arguments, but got {}", expected, got),
            InterpreterErrorKind::IncorrectType => 
                write!(f, "incorrect argument type"),
            InterpreterErrorKind::InvalidAssignmentTarget =>
                write!(f, "invalid assignment target"),

            InterpreterErrorKind::VariantAccessOnNonEnum =>
                write!(f, "cannot access variants of non-enum"),
            InterpreterErrorKind::IncorrectVariantParameters =>
                write!(f, "incorrect arguments to enum variant constructor"),
        
            InterpreterErrorKind::FuncDefinitionInvalidContext =>
                write!(f, "function definitions are not allowed here"),
            InterpreterErrorKind::DuplicateTypeDefinition(name) =>
                write!(f, "type named `{}` is already defined", name),
            InterpreterErrorKind::UseInvalidContext =>
                write!(f, "mixin use is not allowed here"),
            InterpreterErrorKind::UseNonMixin(name) =>
                write!(f, "cannot use `{}` because it is not a mixin", name),

            InterpreterErrorKind::PatternMatchFailed(_, _) =>
                write!(f, "pattern match failed"), // TODO: more details

            InterpreterErrorKind::InternalTestFailed(name) =>
                write!(f, "internal test `{}` failed", name),

            InterpreterErrorKind::ProgramError(message) =>
                f.write_str(message),

            InterpreterErrorKind::Throw(value) =>
                write!(f, "uncaught throw of `{}`", value.borrow().to_language_string()),
        }
    }
}
