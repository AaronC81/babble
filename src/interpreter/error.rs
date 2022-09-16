use crate::source::Location;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpreterError {
    MissingMethod(String),
    MissingName(String, Location),
    MissingCaptureName(String),
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
}
