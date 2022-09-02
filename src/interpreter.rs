use std::{rc::Rc, fmt::Debug, cell::RefCell};

use crate::{parser::{Node, NodeKind, Parser}, source::Location, stdlib::StandardLibrary, tokenizer::Tokenizer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub type_instance: TypeInstance,
}

impl Value {
    pub fn new_integer(value: i64) -> Self {
        Self { type_instance: TypeInstance::PrimitiveInteger(value) }
    }

    pub fn rc(self) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(self))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInstance {
    Fields {
        source_type: Rc<Type>,
        field_values: Vec<Box<Value>>,
    },
    PrimitiveInteger(i64),
}

impl TypeInstance {
    pub fn get_type(&self, stdlib: &StandardLibrary) -> Rc<Type> {
        match self {
            TypeInstance::Fields { source_type, .. } => source_type.clone(),
            TypeInstance::PrimitiveInteger(_) => stdlib.integer.clone(),
        }
    }
}

#[derive(Debug)]
pub struct Type {
    pub id: String,
    pub fields: Vec<String>,
    pub methods: Vec<InternalMethod>,
}
impl PartialEq for Type { fn eq(&self, other: &Self) -> bool { self.id == other.id } }
impl Eq for Type {}

impl Type {
    pub fn resolve_method(&self, name: &str) -> Option<&InternalMethod> {
        self.methods.iter().find(|m| m.name == name)
    }   
}

pub struct InternalMethod {
    pub name: String,
    pub function: Box<dyn Fn(Rc<RefCell<Value>>, Vec<Rc<RefCell<Value>>>) -> Result<Rc<RefCell<Value>>, InterpreterError>>,
}
impl Debug for InternalMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InternalMethod").field("name", &self.name).finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpreterError {
    MissingMethod(String, Location),
}

pub struct Interpreter {
    stdlib: StandardLibrary,
    types: Vec<Rc<Type>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stdlib: StandardLibrary::new(),
            types: vec![],
        }
    }

    pub fn evaluate(&mut self, node: &Node) -> Result<Rc<RefCell<Value>>, InterpreterError> {
        match &node.kind {
            // TODO: safe cast
            NodeKind::IntegerLiteral(i) => Ok(Value::new_integer(*i as i64).rc()),

            NodeKind::SendMessage { receiver, components } => {
                let receiver = self.evaluate(&receiver)?;
                let receiver_ref = receiver.borrow();

                let method_name = components.to_method_name();
                if let Some(method) = receiver_ref.type_instance.get_type(&self.stdlib).resolve_method(&method_name) {
                    let parameters = match components {
                        crate::parser::SendMessageComponents::Unary(_) => vec![],
                        crate::parser::SendMessageComponents::Parameterised(params) =>
                            params.iter().map(|(_, p)| self.evaluate(p)).collect::<Result<Vec<_>, _>>()?,
                    };
                    drop(receiver_ref);
                    (method.function)(receiver, parameters)
                } else {
                    Err(InterpreterError::MissingMethod(method_name, node.location))
                }
            },
        }
    }
}

#[test]
fn test_simple_interpret() {
    let node = Parser::parse(&Tokenizer::tokenize("32 add: 24.").unwrap()[..]).unwrap();
    assert_eq!(
        Interpreter::new().evaluate(&node).unwrap(),
        Value { type_instance: TypeInstance::PrimitiveInteger(56) }.rc(),
    );
}
