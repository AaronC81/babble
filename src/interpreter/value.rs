use std::{rc::Rc, cell::RefCell};

use super::{Type, Interpreter, InterpreterError, Block};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub type_instance: TypeInstance,
}

pub type ValueRef = Rc<RefCell<Value>>;

impl Value {
    pub fn new_integer(value: i64) -> Self {
        Self { type_instance: TypeInstance::PrimitiveInteger(value) }
    }

    pub fn new_type(t: Rc<Type>) -> Self {
        Self { type_instance: TypeInstance::Type(t) }
    }

    pub fn new_null() -> Self {
        Self { type_instance: TypeInstance::PrimitiveNull }
    }

    pub fn new_block(block: Block) -> Self {
        Self { type_instance: TypeInstance::Block(block) }
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

    pub fn to_language_string(&self) -> String {
        match &self.type_instance {
            TypeInstance::Fields { source_type, field_values } => {
                let mut result = source_type.id.clone();
                for (field, value) in source_type.fields.iter().zip(field_values.iter()) {
                    result.push(' ');
                    result.push_str(field);
                    result.push_str(": ");

                    // If the value contains any whitespace, wrap it in parentheses
                    let mut value_str = value.to_language_string();
                    if value_str.chars().any(|c| c.is_whitespace()) {
                        value_str.insert(0, '(');
                        value_str.push(')');
                    }
                    result.push_str(&value_str);
                }

                result
            }

            TypeInstance::Block(_) => "(anonymous block)".into(),
            TypeInstance::Type(t) => t.id.clone(),
            TypeInstance::PrimitiveInteger(i) => i.to_string(),
            TypeInstance::PrimitiveNull => "null".into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInstance {
    Fields {
        source_type: Rc<Type>,
        field_values: Vec<Box<Value>>,
    },
    Type(Rc<Type>),
    Block(Block),
    PrimitiveInteger(i64),
    PrimitiveNull,
}

impl TypeInstance {
    pub fn get_type(&self, interpreter: &Interpreter) -> Rc<Type> {
        match self {
            TypeInstance::Fields { source_type, .. } => source_type.clone(),
            TypeInstance::Type(_) => interpreter.resolve_stdlib_type("Null"), // TODO: should probably be a `Type` type
            // TODO: add create_or_resolve_block_type - this won't work when we have blocks with
            // parameters
            TypeInstance::Block(_) => interpreter.resolve_stdlib_type("Block0"), 
            TypeInstance::PrimitiveInteger(_) => interpreter.resolve_stdlib_type("Integer"),
            TypeInstance::PrimitiveNull => interpreter.resolve_stdlib_type("Null"),
        }
    }
}
