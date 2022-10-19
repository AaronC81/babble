//! Implements values, the core pieces of data which the interpreter works with.

use std::{rc::Rc, cell::RefCell};

use super::{Interpreter, InterpreterErrorKind, Block, TypeData, Variant, TypeRef, InterpreterError};

/// A value within the interpreter, which is an instance of some type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub type_instance: TypeInstance,
}

pub type ValueRef = Rc<RefCell<Value>>;

impl Value {
    /// Constructs a new value from an integer.
    pub fn new_integer(value: i64) -> Self {
        Self { type_instance: TypeInstance::PrimitiveInteger(value) }
    }

    /// Constructs a new value from a string.
    pub fn new_string(value: &str) -> Self {
        Self { type_instance: TypeInstance::PrimitiveString(value.into()) }
    }

    /// Constructs a new value from an array of other values.
    pub fn new_array(values: &[ValueRef]) -> Self {
        Self { type_instance: TypeInstance::PrimitiveArray(values.iter().cloned().collect()) }
    }

    /// Constructs a new value which represents a reference to a type.
    pub fn new_type(t: TypeRef) -> Self {
        Self { type_instance: TypeInstance::Type(t) }
    }

    /// Constructs a new null value.
    pub fn new_null() -> Self {
        Self { type_instance: TypeInstance::PrimitiveNull }
    }

    /// Constructs a new block value.
    pub fn new_block(block: Block) -> Self {
        Self { type_instance: TypeInstance::Block(block) }
    }

    /// Constructs a new boolean value.
    /// 
    /// Unlike most, this constructor requires an interpreter reference, because `Boolean` is
    /// defined in the Babble standard library as an enum rather than being a core type, so must be
    /// resolved from an instantiated standard library.
    pub fn new_boolean(interpreter: &Interpreter, value: bool) -> Self {
        let boolean_type = interpreter.resolve_stdlib_type("Boolean");
        let boolean_type_ref = boolean_type.borrow();
        let variant_name = if value { "True" } else { "False" };

        Self {
            type_instance: TypeInstance::Fields {
                source_type: boolean_type.clone(),
                variant: Some(boolean_type_ref.resolve_variant(variant_name).unwrap().0),
                field_values: vec![],
            }
        }
    }

    /// Transforms this into a [ValueRef].
    pub fn rc(self) -> ValueRef {
        Rc::new(RefCell::new(self))
    }

    /// Extracts the integer from this value, or returns an error if it is not an integer.
    pub fn to_integer(&self) -> Result<i64, InterpreterError> {
        if let TypeInstance::PrimitiveInteger(i) = self.type_instance {
            Ok(i)
        } else {
            Err(InterpreterErrorKind::IncorrectType.into())
        }
    }

    /// Extracts the string from this value, or returns an error if it is not an string.
    pub fn to_string(&self) -> Result<String, InterpreterError> {
        if let TypeInstance::PrimitiveString(string) = &self.type_instance {
            Ok(string.clone())
        } else {
            Err(InterpreterErrorKind::IncorrectType.into())
        }
    }

    /// Extracts the array from this value, or returns an error if it is not an array.
    pub fn to_array(&mut self) -> Result<&mut Vec<ValueRef>, InterpreterError> {
        if let TypeInstance::PrimitiveArray(ref mut array) = &mut self.type_instance {
            Ok(array)
        } else {
            Err(InterpreterErrorKind::IncorrectType.into())
        }
    }

    /// Extracts the boolean from this value, or returns an error if it is not a boolean.
    /// 
    /// This does not perform a truthy or falsy conversion. Babble (currently) has no context of
    /// truthy or falsy, since boolean methods are only available on instances of the `Boolean`
    /// type.
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
            Err(InterpreterErrorKind::IncorrectType.into())
        }
    }

    /// Extracts the block from this value, or returns an error if it is not a block.
    pub fn to_block(&self) -> Result<&Block, InterpreterError> {
        if let TypeInstance::Block(b) = &self.type_instance {
            Ok(b)
        } else {
            Err(InterpreterErrorKind::IncorrectType.into())
        }
    }

    /// Extracts the type reference from this value, or returns an error if it is not a type.
    /// 
    /// (This does not get the _type_ of this value - rather, it assumes that this value _is_ a
    ///  reference to a type, and returns that type.)
    pub fn to_type(&self) -> Result<TypeRef, InterpreterError> {
        if let TypeInstance::Type(t) = &self.type_instance {
            Ok(t.clone())
        } else {
            Err(InterpreterErrorKind::IncorrectType.into())
        }
    }

    /// Converts this value into a printable string.
    pub fn to_language_string(&self) -> String {
        match &self.type_instance {
            TypeInstance::Fields { source_type, variant, field_values } => {
                let source_type_ref = source_type.borrow();
                let (fields, variant) = if let Some(variant) = variant {
                    if let TypeData::Variants(variants) = &source_type_ref.data {
                        let Variant { name, fields } = &variants[*variant];
                        (fields, Some(name))
                    } else {
                        unreachable!()
                    }
                } else if let TypeData::Fields(f) = &source_type_ref.data {
                    (f, None)
                } else {
                    unreachable!()
                };

                let mut result = source_type.borrow().id.clone();
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
            TypeInstance::Type(t) => t.borrow().id.clone(),
            TypeInstance::PrimitiveInteger(i) => i.to_string(),
            TypeInstance::PrimitiveString(s) => s.clone(),
            TypeInstance::PrimitiveArray(a) => "(array)".into(), // TODO: decide how to lay these out
            TypeInstance::PrimitiveNull => "null".into(),
        }
    }

    /// Performs a "soft copy" of this value, cloning primitive values and returning compound types
    /// unchanged.
    pub fn soft_copy(value: Rc<RefCell<Self>>) -> Rc<RefCell<Value>> {
        match value.borrow().type_instance {
            TypeInstance::Fields { .. }
            | TypeInstance::Type(_)
            | TypeInstance::Block(_) => value.clone(),

            TypeInstance::PrimitiveInteger(i) => Value::new_integer(i).rc(),
            TypeInstance::PrimitiveString(ref s) => Value::new_string(s).rc(),
            TypeInstance::PrimitiveArray(ref a) => Value::new_array(a).rc(),
            TypeInstance::PrimitiveNull => Value::new_null().rc(),
        }
    }
}

/// An instance of a type, either defined or primitive, which composes the data behind a value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInstance {
    /// An instance of a struct or enum, containing a sequence of fields, and optionally a variant
    /// index if the type is an enum.
    /// 
    /// These fields and variants are all unnamed - the names must be resolved through the type.
    Fields {
        source_type: TypeRef,
        variant: Option<usize>,
        field_values: Vec<ValueRef>,
    },

    /// The value is a reference to a type.
    Type(TypeRef),

    /// The value is a block.
    Block(Block),

    /// The value is an integer.
    PrimitiveInteger(i64),

    /// The value is a string.
    PrimitiveString(String),

    /// The value is an array.
    PrimitiveArray(Vec<ValueRef>),

    /// The value is null.
    PrimitiveNull,
}

impl TypeInstance {
    /// Gets the type of the value represented by this type instance.
    /// 
    /// For example, a [`TypeInstance::PrimitiveInteger`] would return a reference to the
    /// `Integer` type. 
    pub fn get_type(&self, interpreter: &Interpreter) -> TypeRef {
        match self {
            TypeInstance::Fields { source_type, .. } => source_type.clone(),
            TypeInstance::Type(_) => interpreter.resolve_stdlib_type("Null"), // TODO: should probably be a `Type` type
            TypeInstance::Block(_) => interpreter.resolve_stdlib_type("Block"), 
            TypeInstance::PrimitiveInteger(_) => interpreter.resolve_stdlib_type("Integer"),
            TypeInstance::PrimitiveString(_) => interpreter.resolve_stdlib_type("String"),
            TypeInstance::PrimitiveArray(_) => interpreter.resolve_stdlib_type("Array"),
            TypeInstance::PrimitiveNull => interpreter.resolve_stdlib_type("Null"),
        }
    }
}
