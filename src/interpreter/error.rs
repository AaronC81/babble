use std::fmt::Display;

use crate::{source::Location, parser::Node};

use super::{ValueRef, StackFrame, Interpreter};

#[derive(Debug, Clone)]
pub struct InterpreterError {
    pub kind: InterpreterErrorKind,
    pub details: Option<InterpreterErrorDetails>,
}

impl InterpreterError {
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

#[derive(Debug, Clone)]
pub struct InterpreterErrorDetails {
    pub location: Option<Location>,
    pub backtrace: Vec<StackFrame>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpreterErrorKind {
    MissingMethod(ValueRef, String),
    MissingName(String),
    MissingCaptureName(String),
    MissingVariant(String, String),
    IntegerOverflow,

    IncorrectArity {
        name: String,
        expected: usize,
        got: usize,
    },
    IncorrectBlockArity {
        expected: usize,
        got: usize,
    },
    IncorrectType, // TODO more details
    InvalidAssignmentTarget,

    VariantAccessOnNonEnum,
    IncorrectVariantParameters,

    FuncDefinitionInvalidContext,
    DuplicateTypeDefinition(String),
    UseInvalidContext,
    UseNonMixin(String),

    InternalTestFailed(String),

    ProgramError(String),
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

            InterpreterErrorKind::InternalTestFailed(name) =>
                write!(f, "internal test `{}` failed", name),

            InterpreterErrorKind::ProgramError(message) =>
                f.write_str(message),
        }
    }
}
