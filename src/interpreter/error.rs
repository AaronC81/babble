use crate::source::Location;

use super::ValueRef;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpreterError {
    MissingMethod(ValueRef, String),
    MissingName(String, Location),
    MissingCaptureName(String),
    MissingVariant(String),
    IntegerOverflow(Location),

    // TODO: location for these
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
    InvalidAssignmentTarget(Location),

    VariantAccessOnNonEnum,
    IncorrectVariantParameters,

    FuncDefinitionInvalidContext,
    DuplicateTypeDefinition(String),
    UseInvalidContext,
    UseNonMixin(String),

    InternalTestFailed(String),
}
