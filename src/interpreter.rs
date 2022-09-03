use std::{rc::Rc, fmt::Debug, cell::RefCell};
use crate::{parser::{Node, NodeKind}, source::Location, stdlib::StandardLibrary};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub type_instance: TypeInstance,
}

pub type ValueRef = Rc<RefCell<Value>>;

impl Value {
    pub fn new_integer(value: i64) -> Self {
        Self { type_instance: TypeInstance::PrimitiveInteger(value) }
    }

    pub fn new_null() -> Self {
        Self { type_instance: TypeInstance::PrimitiveNull }
    }

    pub fn rc(self) -> ValueRef {
        Rc::new(RefCell::new(self))
    }

    pub fn to_integer(&self) -> Result<i64, InterpreterError> {
        if let TypeInstance::PrimitiveInteger(i) = self.type_instance {
            Ok(i)
        } else {
            Err(InterpreterError::IncorrectType)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInstance {
    Fields {
        source_type: Rc<Type>,
        field_values: Vec<Box<Value>>,
    },
    PrimitiveInteger(i64),
    PrimitiveNull,
}

impl TypeInstance {
    pub fn get_type(&self, stdlib: &StandardLibrary) -> Rc<Type> {
        match self {
            TypeInstance::Fields { source_type, .. } => source_type.clone(),
            TypeInstance::PrimitiveInteger(_) => stdlib.integer.clone(),
            TypeInstance::PrimitiveNull => stdlib.null.clone(),
        }
    }
}

#[derive(Debug)]
pub struct Type {
    pub id: String,
    pub fields: Vec<String>,
    pub methods: Vec<InternalMethodRef>,
}
impl PartialEq for Type { fn eq(&self, other: &Self) -> bool { self.id == other.id } }
impl Eq for Type {}

impl Type {
    pub fn resolve_method(&self, name: &str) -> Option<InternalMethodRef> {
        self.methods.iter().find(|m| m.name == name).cloned()
    }   
}

pub struct InternalMethod {
    pub name: String,
    function: Box<dyn Fn(ValueRef, Vec<ValueRef>) -> InterpreterResult>,
}
impl Debug for InternalMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InternalMethod").field("name", &self.name).finish()
    }
}

type InternalMethodRef = Rc<InternalMethod>;

impl InternalMethod {
    pub fn new<F>(name: &str, function: F) -> Self
    where F: Fn(ValueRef, Vec<ValueRef>) -> InterpreterResult + 'static {
        Self {
            name: name.into(),
            function: Box::new(function),
        }
    }
    
    pub fn rc(self) -> InternalMethodRef {
        Rc::new(self)
    }

    pub fn arity(&self) -> usize {
        self.name.matches(":").count()
    }

    pub fn call(&self, receiver: ValueRef, parameters: Vec<ValueRef>) -> InterpreterResult {
        if self.arity() != parameters.len() {
            return Err(InterpreterError::IncorrectArity {
                name: self.name.clone(),
                expected: self.arity(),
                got: parameters.len(),
            })
        }

        (self.function)(receiver, parameters)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpreterError {
    MissingMethod(String, Location),
    IntegerOverflow(Location),

    // TODO: location for these
    IncorrectArity {
        name: String,
        expected: usize,
        got: usize,
    },
    IncorrectType, // TODO more details
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexicalContext {
    parent: Option<LexicalContextRef>,
    locals: Vec<(String, ValueRef)>,
}

impl LexicalContext {
    pub fn new_top_level() -> Self {
        LexicalContext { parent: None, locals: vec![] }
    }

    pub fn new_with_parent(parent: LexicalContextRef) -> Self {
        LexicalContext { parent: Some(parent), locals: vec![] }
    }

    pub fn rc(self) -> LexicalContextRef {
        Rc::new(RefCell::new(self))
    }
}

pub type LexicalContextRef = Rc<RefCell<LexicalContext>>;

pub struct StackFrame {
    context: InternalMethodRef,
}

pub struct Interpreter {
    stdlib: StandardLibrary,
    types: Vec<Rc<Type>>,
    stack: Vec<StackFrame>,
}

pub type InterpreterResult = Result<ValueRef, InterpreterError>;

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stdlib: StandardLibrary::new(),
            types: vec![],
            stack: vec![],
        }
    }

    pub fn evaluate(&mut self, node: &Node) -> InterpreterResult {
        match &node.kind {
            NodeKind::IntegerLiteral(i) => (*i).try_into()
                .map(|i| Value::new_integer(i).rc())
                .map_err(|_| InterpreterError::IntegerOverflow(node.location)),

            NodeKind::SendMessage { receiver, components } => {
                // Evaluate the receiver
                let receiver = self.evaluate(&receiver)?;
                let receiver_ref = receiver.borrow();

                let method_name = components.to_method_name();
                if let Some(method) = receiver_ref.type_instance.get_type(&self.stdlib).resolve_method(&method_name) {
                    drop(receiver_ref);

                    // Evaluate parameters
                    let parameters = match components {
                        crate::parser::SendMessageComponents::Unary(_) => vec![],
                        crate::parser::SendMessageComponents::Parameterised(params) =>
                            params.iter().map(|(_, p)| self.evaluate(p)).collect::<Result<Vec<_>, _>>()?,
                    };

                    // Create a new stack frame, call the method within it, and pop the frame
                    self.stack.push(StackFrame {
                        context: method.clone(),
                    });
                    let result = (method.function)(receiver, parameters);
                    self.stack.pop();

                    result
                } else {
                    Err(InterpreterError::MissingMethod(method_name, node.location))
                }
            },

            NodeKind::StatementSequence(seq) => {
                // Bodies return their last statement, except if they have no statements, in which
                // case they return null
                let mut result = Value::new_null().rc();
                for node in seq {
                    result = self.evaluate(node)?;
                }
                Ok(result)
            }
        }
    }
}

mod tests {
    use crate::{parser::Parser, tokenizer::Tokenizer, interpreter::{TypeInstance, Interpreter, InterpreterError, Value}};

    use super::ValueRef;

    fn evaluate(input: &str) -> Result<ValueRef, InterpreterError> {
        let node = Parser::parse(&Tokenizer::tokenize(input).unwrap()[..]).unwrap();
        Interpreter::new().evaluate(&node)
    }

    #[test]
    fn test_simple_interpret() {
        assert_eq!(
            evaluate("32 add: 24.").unwrap(),
            Value::new_integer(56).rc(),
        );
    }

    #[test]
    fn test_integer_overflow_on_token_conversion() {
        assert!(matches!(
            //   One more than the maximum i64
            //        vvvvvvvvvvvvvvvvvvv
            evaluate("9223372036854775808."),
            Err(InterpreterError::IntegerOverflow(..)),
        ));
    }

    #[test]
    fn test_message_precedence() {
        assert_eq!(
            evaluate("2 negate add: 7 negate.").unwrap(),
            Value::new_integer(-9).rc(),
        )
    }

    #[test]
    fn test_sequence() {
        // A sequence evaluates to its last item
        assert_eq!(
            evaluate("1. 2. 3.").unwrap(),
            Value::new_integer(3).rc(),
        )
    }
}
