//! Implements values, the core pieces of data which the interpreter works with.

use std::{rc::Rc, cell::{RefCell, Ref, RefMut}, any::Any, fmt::Debug, ops::{Deref, DerefMut}};

use crate::parser::BlockParameters;

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
        Self { type_instance: TypeInstance::PrimitiveArray(values.to_vec()) }
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

    /// Constructs a new match value, with `Hit` or `Miss` being mapped to the given `Option`.
    /// 
    /// Unlike most, this constructor requires an interpreter reference, because `Match` is
    /// defined in the Babble standard library as an enum rather than being a core type, so must be
    /// resolved from an instantiated standard library.
    pub fn new_match(interpreter: &Interpreter, value: Option<ValueRef>) -> Self {
        let match_type = interpreter.resolve_stdlib_type("Match");
        let match_type_ref = match_type.borrow();

        let variant_name;
        let field_values;
        if let Some(value) = value {
            variant_name = "Hit";
            field_values = vec![value];
        } else {
            variant_name = "Miss";
            field_values = vec![];
        }

        Self {
            type_instance: TypeInstance::Fields {
                source_type: match_type.clone(),
                variant: Some(match_type_ref.resolve_variant(variant_name).unwrap().0),
                field_values,
            }
        }
    }

    /// Constructs an instance of another primitive value which implements `PrimitiveValue`. The
    /// value can be accessed through downcasting using [`Value::to_other`] or
    /// [`Value::to_other_mut`].
    pub fn new_other<T: PrimitiveValue>(value: T) -> Self {
        Self { type_instance: TypeInstance::PrimitiveOther(Rc::new(RefCell::new(value))) }
    }

    /// Transforms this into a [ValueRef].
    pub fn rc(self) -> ValueRef {
        Rc::new(RefCell::new(self))
    }

    /// Extracts the integer from this value, or returns an error if it is not an integer.
    pub fn as_integer(&self) -> Result<i64, InterpreterError> {
        if let TypeInstance::PrimitiveInteger(i) = self.type_instance {
            Ok(i)
        } else {
            Err(InterpreterErrorKind::IncorrectType.into())
        }
    }

    /// Extracts the string from this value, or returns an error if it is not an string.
    pub fn as_string(&self) -> Result<String, InterpreterError> {
        if let TypeInstance::PrimitiveString(string) = &self.type_instance {
            Ok(string.clone())
        } else {
            Err(InterpreterErrorKind::IncorrectType.into())
        }
    }

    /// Extracts the array from this value, or returns an error if it is not an array.
    pub fn as_array(&mut self) -> Result<&mut Vec<ValueRef>, InterpreterError> {
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

    /// Extracts the "other" primitive from this value, and downcasts it to the given type parameter
    /// `T` which implements [PrimitiveValue].
    /// 
    /// If this value is not a "other" primitive, or if the downcast fails, returns an error.
    pub fn as_other<T: PrimitiveValue>(&self) -> Result<impl Deref<Target = T> + '_, InterpreterError> {
        let TypeInstance::PrimitiveOther(other) = &self.type_instance else {
            return Err(InterpreterErrorKind::IncorrectType.into())
        };
        let r = other.borrow() as Ref<dyn Any>;

        // Check we can actually downcast first, and bail with an Err if we can't...
        r.downcast_ref::<T>().ok_or(InterpreterErrorKind::IncorrectType.into())?;

        // ...then return a mapped Ref to the downcasted value if we can
        // (Because we already checked, the unwrap won't panic)
        Ok(Ref::map(r, |other| { other.downcast_ref::<T>().unwrap() }))
    }

    /// Mutable version of [`Value::as_other`].
    pub fn as_other_mut<T: PrimitiveValue>(&self) -> Result<impl DerefMut<Target = T> + '_, InterpreterError> {
        let TypeInstance::PrimitiveOther(other) = &self.type_instance else {
            return Err(InterpreterErrorKind::IncorrectType.into())
        };           
        let mut r = other.borrow_mut() as RefMut<dyn Any>;

        // Check we can actually downcast first, and bail with an Err if we can't...
        r.downcast_mut::<T>().ok_or(InterpreterErrorKind::IncorrectType.into())?;

        // ...then return a mapped RefMut to the downcasted value if we can
        // (Because we already checked, the unwrap won't panic)
        Ok(RefMut::map(r, |other| { other.downcast_mut::<T>().unwrap() }))
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
                } else if let TypeData::Fields { instance_fields, .. } = &source_type_ref.data {
                    (instance_fields, None)
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

            TypeInstance::Block(b) => match b.parameters {
                BlockParameters::Named(_) | BlockParameters::All(_) => "[ block ]".into(),
                BlockParameters::Patterned { fatal, .. } => if fatal {
                    "![ block ]".into()
                } else {
                    "?[ block ]".into()
                },
            }
            TypeInstance::Type(t) => t.borrow().id.clone(),
            TypeInstance::PrimitiveInteger(i) => i.to_string(),
            TypeInstance::PrimitiveString(s) => s.clone(),
            TypeInstance::PrimitiveArray(a) =>
                if a.is_empty() {
                    "#{ }".to_string()
                } else {
                    format!("#{{ {} }}", a.iter()
                        .map(|v| v.borrow().to_ws_safe_language_string())
                        .collect::<Vec<_>>()
                        .join(" "))
                },
            TypeInstance::PrimitiveNull => "null".into(),
            TypeInstance::PrimitiveOther(v) => v.borrow().to_language_string(),
        }
    }

    /// The same as `to_language_string`, but wraps this value in parentheses if it contains any
    /// whitespace.
    pub fn to_ws_safe_language_string(&self) -> String {
        let mut result = self.to_language_string();
        if result.chars().any(|c| c.is_whitespace()) {
            result.insert(0, '(');
            result.push(')');
        }
        result
    }

    /// Performs a "soft copy" of this value, cloning primitive values and returning compound types
    /// unchanged.
    pub fn soft_copy(value: Rc<RefCell<Self>>) -> Rc<RefCell<Value>> {
        match value.borrow().type_instance {
            TypeInstance::Fields { .. }
            | TypeInstance::Type(_)
            | TypeInstance::Block(_)
            | TypeInstance::PrimitiveOther(_)
            | TypeInstance::PrimitiveArray(_) => value.clone(),

            TypeInstance::PrimitiveInteger(i) => Value::new_integer(i).rc(),
            TypeInstance::PrimitiveString(ref s) => Value::new_string(s).rc(),
            TypeInstance::PrimitiveNull => Value::new_null().rc(),
        }
    }
}

/// An instance of a type, either defined or primitive, which composes the data behind a value.
#[derive(Debug, Clone)]
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

    /// The value is an instance of another native type which implements [PrimitiveValue].
    PrimitiveOther(Rc<RefCell<dyn PrimitiveValue>>), 

    /// The value is null.
    PrimitiveNull,
}

impl PartialEq for TypeInstance {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeInstance::Fields { source_type: a, variant: a_variant, field_values: a_fields },
             TypeInstance::Fields { source_type: b, variant: b_variant, field_values: b_fields }) => {
                a == b && a_variant == b_variant && a_fields == b_fields
            }
            (TypeInstance::Type(a), TypeInstance::Type(b)) => a == b,
            (TypeInstance::Block(a), TypeInstance::Block(b)) => a == b,

            (TypeInstance::PrimitiveInteger(a), TypeInstance::PrimitiveInteger(b)) => a == b,
            (TypeInstance::PrimitiveString(a), TypeInstance::PrimitiveString(b)) => a == b,
            (TypeInstance::PrimitiveArray(a), TypeInstance::PrimitiveArray(b)) => a == b,
            (TypeInstance::PrimitiveNull, TypeInstance::PrimitiveNull) => true,

            (TypeInstance::PrimitiveOther(a), TypeInstance::PrimitiveOther(b)) => Rc::ptr_eq(a, b),

            _ => false,
        }
    }
}
impl Eq for TypeInstance {}

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
            TypeInstance::PrimitiveOther(v) => v.borrow().get_type(interpreter),
            TypeInstance::PrimitiveNull => interpreter.resolve_stdlib_type("Null"),
        }
    }
}

/// Can be implemented on any type to allow it to be used as a [TypeInstance::PrimitiveOther].
pub trait PrimitiveValue: Any + Debug {
    /// Gets the type of this value in the language.
    fn get_type(&self, interpreter: &Interpreter) -> TypeRef;

    /// Converts this value to a string.
    fn to_language_string(&self) -> String;
}
