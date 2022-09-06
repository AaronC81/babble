use crate::source::Location;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpreterError {
    MissingMethod(String, Location),
    MissingName(String, Location),
    IntegerOverflow(Location),

    // TODO: location for these
    IncorrectArity {
        name: String,
        expected: usize,
        got: usize,
    },
    IncorrectType, // TODO more details
    InvalidAssignmentTarget(Location),
}
