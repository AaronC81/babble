use std::{rc::Rc, cell::RefCell};

use super::{Type, Interpreter, InterpreterError, Block, TypeData, Variant};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub type_instance: TypeInstance,
}

pub type ValueRef = Rc<RefCell<Value>>;

impl Value {
    pub fn new_integer(value: i64) -> Self {
        Self { type_instance: TypeInstance::PrimitiveInteger(value) }
    }

    pub fn new_string(value: &str) -> Self {
        Self { type_instance: TypeInstance::PrimitiveString(value.into()) }
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

    pub fn new_boolean(interpreter: &Interpreter, value: bool) -> Self {
        let boolean_type = interpreter.resolve_stdlib_type("Boolean");
        let variant_name = if value { "True" } else { "False" };

        Self {
            type_instance: TypeInstance::Fields {
                source_type: boolean_type.clone(),
                variant: Some(boolean_type.resolve_variant(variant_name).unwrap().0),
                field_values: vec![],
            }
        }
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

    pub fn to_boolean(&self) -> Result<bool, InterpreterError> {
        if let TypeInstance::Fields { variant, .. } = self.type_instance {
            // A bit naughty to just compare variant indexes - but we defined the
            // variant, so we can be reasonably confident
            match variant {
                Some(0) => Ok(false),
                Some(1) => Ok(true),
                _ => unreachable!()
            }
        } else {
            Err(InterpreterError::IncorrectType)
        }
    }

    pub fn to_block(&self) -> Result<&Block, InterpreterError> {
        if let TypeInstance::Block(b) = &self.type_instance {
            Ok(b)
        } else {
            Err(InterpreterError::IncorrectType)
        }
    }

    pub fn to_language_string(&self) -> String {
        match &self.type_instance {
            TypeInstance::Fields { source_type, variant, field_values } => {
                let (fields, variant) = if let Some(variant) = variant {
                    if let TypeData::Variants(variants) = &source_type.data {
                        let Variant { name, fields } = &variants[*variant];
                        (fields, Some(name))
                    } else {
                        unreachable!()
                    }
                } else {
                    if let TypeData::Fields(f) = &source_type.data {
                        (f, None)
                    } else {
                        unreachable!()
                    }
                };

                let mut result = source_type.id.clone();
                if let Some(variant) = variant {
                    result.push('#');
                    result.push_str(variant);
                }
                for (field, value) in fields.iter().zip(field_values.iter()) {
                    result.push(' ');
                    result.push_str(field);
                    result.push_str(": ");

                    // If the value contains any whitespace, wrap it in parentheses
                    let mut value_str = value.borrow().to_language_string();
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
            TypeInstance::PrimitiveString(s) => s.clone(),
            TypeInstance::PrimitiveNull => "null".into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInstance {
    Fields {
        source_type: Rc<Type>,
        variant: Option<usize>,
        field_values: Vec<ValueRef>,
    },
    Type(Rc<Type>),
    Block(Block),
    PrimitiveInteger(i64),
    PrimitiveString(String),
    PrimitiveNull,
}

impl TypeInstance {
    pub fn get_type(&self, interpreter: &Interpreter) -> Rc<Type> {
        match self {
            TypeInstance::Fields { source_type, .. } => source_type.clone(),
            TypeInstance::Type(_) => interpreter.resolve_stdlib_type("Null"), // TODO: should probably be a `Type` type
            TypeInstance::Block(_) => interpreter.resolve_stdlib_type("Block"), 
            TypeInstance::PrimitiveInteger(_) => interpreter.resolve_stdlib_type("Integer"),
            TypeInstance::PrimitiveString(_) => interpreter.resolve_stdlib_type("String"),
            TypeInstance::PrimitiveNull => interpreter.resolve_stdlib_type("Null"),
        }
    }
}
