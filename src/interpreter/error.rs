//! Models errors which can occur during code execution.

use std::fmt::Display;

use crate::{source::Location, parser::{Pattern, ParserError}, tokenizer::TokenizerError};

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
    pub fn add_details(self, loc: &Location, interpreter: Option<&Interpreter>) -> Self {
        if self.details.is_some() {
            self
        } else {
            Self {
                details: Some(InterpreterErrorDetails {
                    location: Some(loc.clone()),
                    backtrace: interpreter.map(|i| i.stack.clone()),
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
    pub backtrace: Option<Vec<StackFrame>>,
}

/// The core cause of an [InterpreterError].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpreterErrorKind {
    /// The receiver of a call did not have a method to satisfy the call.
    MissingMethod(ValueRef, String),

    /// The method cannot be called from here, because it is private.
    PrivateMethod(ValueRef, String),

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

    /// An error occurred while performing IO from user code.
    IoError(String),

    /// An internal error caused by a block of instructions not leaving exactly 1 value on the value
    /// stack.
    StackImbalance(usize),

    /// Not really an error - used by `Program throw: ...` to unwind the stack
    Throw(ValueRef),

    /// Not really an error - used to indicate that a method must be handled magically
    Magic,

    /// This interpreter error was actually a tokenizer error.
    TokenizerError(TokenizerError),

    /// This interpreter error was actually a parser error.
    ParserError(ParserError),
}

impl InterpreterErrorKind {
    pub fn is_fatal(&self) -> bool {
        !matches!(self, InterpreterErrorKind::Throw(_))
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
            InterpreterErrorKind::PrivateMethod(val, m) =>
                write!(f, "cannot call private method `{}` on `{}`", m, val.borrow().to_language_string()),
            InterpreterErrorKind::MissingName(name) => 
                write!(f, "`{name}` does not exist"),
            InterpreterErrorKind::MissingCaptureName(name) =>
                write!(f, "`{name}` cannot be captured because it does not exist"),
            InterpreterErrorKind::MissingVariant(type_id, var) =>
                write!(f, "enum `{type_id}` has no variant `{var}`"),
            InterpreterErrorKind::IntegerOverflow =>
                write!(f, "integer overflow"),

            InterpreterErrorKind::IncorrectArity { name, expected, got } =>
                write!(f, "method `{name}` expects {expected} arguments, but got {got}"),
            InterpreterErrorKind::IncorrectBlockArity { expected, got } =>
                write!(f, "block expected {expected} arguments, but got {got}"),
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
                write!(f, "type named `{name}` is already defined"),
            InterpreterErrorKind::UseInvalidContext =>
                write!(f, "mixin use is not allowed here"),
            InterpreterErrorKind::UseNonMixin(name) =>
                write!(f, "cannot use `{name}` because it is not a mixin"),

            InterpreterErrorKind::PatternMatchFailed(_, _) =>
                write!(f, "pattern match failed"), // TODO: more details

            InterpreterErrorKind::InternalTestFailed(name) =>
                write!(f, "internal test `{name}` failed"),

            InterpreterErrorKind::ProgramError(message) =>
                f.write_str(message),
            InterpreterErrorKind::IoError(err) =>
                write!(f, "IO error: {err}"),
            InterpreterErrorKind::StackImbalance(n) =>
                write!(f, "internal error: expected 1 item on the value stack, got {n}"),

            InterpreterErrorKind::Throw(value) =>
                write!(f, "uncaught throw of `{}`", value.borrow().to_language_string()),
            InterpreterErrorKind::Magic =>
                write!(f, "internal error: unhandled magic method"),

            InterpreterErrorKind::TokenizerError(e) =>
                write!(f, "tokenizer error: {e:?}"),
            InterpreterErrorKind::ParserError(e) =>
                write!(f, "parser error: {e:?}"),
        }
    }
}
