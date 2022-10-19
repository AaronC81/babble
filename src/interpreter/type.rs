//! Allows types to be defined, with different underlying data layouts.
//! 
//! See [`Type`] for more information.

use std::{rc::Rc, fmt::Debug, cell::RefCell};

use crate::{interpreter::TypeInstance};

use super::{InterpreterErrorKind, Value, Method, MethodRef, MethodLocality, InterpreterError};

/// A type which can be instantiated to create a [`Value`].
#[derive(Debug)]
pub struct Type {
    pub id: String,
    pub data: TypeData,
    pub methods: Vec<MethodRef>,
    pub static_methods: Vec<MethodRef>,
    pub used_mixins: Vec<TypeRef>,
}
impl PartialEq for Type { fn eq(&self, other: &Self) -> bool { self.id == other.id } }
impl Eq for Type {}

impl Type {
    /// Creates a new empty type with the given ID.
    pub fn new(id: &str) -> Self {
        Self {
            id: id.into(),
            data: TypeData::Empty,
            methods: vec![],
            static_methods: vec![],
            used_mixins: vec![],
        }
    }

    /// Resolves a method on this type which matches the given name and locality. Returns a
    /// reference to the method if it exists, or `None` if it couldn't be found.
    /// 
    /// Resolution begins with methods defined on the type itself, and then tries mixins, starting
    /// with the most recently `use`-d.
    pub fn resolve_method(&self, name: &str, locality: MethodLocality) -> Option<MethodRef> {
        let pool = match locality {
            MethodLocality::Instance => &self.methods,
            MethodLocality::Static => &self.static_methods,
        };

        // Try to find methods from this type first
        if let Some(method) = pool.iter().find(|m| m.name == name).cloned() {
            return Some(method)
        }

        // If that didn't work, recurse into mixins
        // (Reverse order - most recently `use`d mixin should be looked at first)
        for mixin in self.used_mixins.iter().rev() {
            if let Some(method) = mixin.borrow().resolve_method(name, locality) {
                return Some(method)
            }
        }

        // Nowhere to be found!
        None
    }

    /// Convenience method for resolving methods with [`MethodLocality::Instance`].
    pub fn resolve_instance_method(&self, name: &str) -> Option<MethodRef> {
        self.resolve_method(name, MethodLocality::Instance)
    }

    /// Convenience method for resolving methods with [`MethodLocality::Static`].
    pub fn resolve_static_method(&self, name: &str) -> Option<MethodRef> {
        self.resolve_method(name, MethodLocality::Static)
    }

    /// Resolves an enum variant by name on this type. Returns an error if this type is not an enum,
    /// or if it is an enum but doesn't have a variant with this name.
    pub fn resolve_variant(&self, name: &str) -> Result<(usize, &Variant), InterpreterError> {
        let TypeData::Variants(variants) = &self.data else {
            return Err(InterpreterErrorKind::VariantAccessOnNonEnum.into());
        };

        if let Some(variant) = variants.iter().enumerate().find(|(_, v)| v.name == name) {
            Ok(variant)
        } else {
            Err(InterpreterErrorKind::MissingVariant(self.id.clone(), name.into()).into())
        }
    }

    /// Adds an instance method to this type, deleting any instance method with the same name first.
    pub fn add_method(&mut self, method: MethodRef) {
        self.methods.retain(|m| m.name != method.name);
        self.methods.push(method);
    }

    /// Adds a static method to this type, deleting any static method with the same name first.
    pub fn add_static_method(&mut self, method: MethodRef) {
        self.static_methods.retain(|m| m.name != method.name);
        self.static_methods.push(method);
    }

    /// Generates methods which allow fields of this type to be accessed.
    /// 
    /// For structs, this generates one method for each field, which returns that field.
    /// 
    /// For enums, this computes the union of fields across all variants, and generates a method for
    /// each. The method returns the field if it exists for the current variant, otherwise it
    /// returns a missing method error, as if the method didn't exist in the first place. This gives
    /// the illusion of a separate set of methods for each variant.
    /// 
    /// For other types, generates nothing.
    pub fn generate_accessor_methods(&mut self) {
        match &self.data {
            TypeData::Fields(fields) => {
                for (i, field) in fields.clone().iter().enumerate() {
                    self.add_method(Method::new_internal(field, move |_, r, _| {
                        let TypeInstance::Fields { field_values, .. } = &(*r).borrow().type_instance else {
                            return Err(InterpreterErrorKind::IncorrectType.into());
                        };

                        Ok(field_values[i].clone())
                    }).rc());
                }
            },

            TypeData::Variants(variants) => {
                // This case is a bit tricker, because different variants might contain a field with
                // the same name, in different positions
                let variants = variants.clone();
                let all_fields = variants.iter()
                    .flat_map(|v| v.fields.clone())
                    .collect::<Vec<_>>();
                
                for field in all_fields.iter().cloned() {
                    let variants_copy = variants.clone();
                    self.add_method(Method::new_internal(&field.clone(), move |_, r, _| {
                        let TypeInstance::Fields { variant, field_values, .. } = &(*r).borrow().type_instance else {
                            return Err(InterpreterErrorKind::IncorrectType.into());
                        };
                        let variant = &variants_copy[variant.unwrap()];

                        if let Some(index) = variant.field_index(&field) {
                            Ok(field_values[index].clone())
                        } else {
                            Err(InterpreterErrorKind::MissingMethod(r.clone(), field.clone()).into())
                        }
                    }).rc());
                }
            }

            TypeData::Empty | TypeData::Mixin => (),
        }
    }

    /// Generates a static constructor method for this type, which takes all of the struct fields in
    /// order, and returns a new instance.
    /// 
    /// **Panics** if this type is not a struct.
    pub fn generate_struct_constructor(t: Rc<RefCell<Type>>) {
        let fields = if let TypeData::Fields(ref fields) = t.as_ref().borrow().data {
            fields.clone()
        } else {
            panic!("can only generate constructor for structs");
        };
        if fields.is_empty() { return }

        let constructor_name = fields.iter()
            .map(|f| format!("{}:", f))
            .collect::<String>();
        t.clone().borrow_mut().add_static_method(Method::new_internal(&constructor_name, move |_, _, a| {
            Ok(Value {
                type_instance: TypeInstance::Fields {
                    source_type: t.clone(),
                    variant: None,
                    field_values: a,
                }
            }.rc())
        }).rc());
    }

    /// Transforms this into a [`TypeRef`].
    pub fn rc(self) -> TypeRef {
        Rc::new(RefCell::new(self))
    }
}

pub type TypeRef = Rc<RefCell<Type>>;

/// The inner data layout for a [`Type`].
#[derive(Debug, Clone)]
pub enum TypeData {
    /// The type has no inner data.
    Empty,

    /// The type is a struct, and its inner data is a sequence of named fields.
    Fields(Vec<String>),

    /// The type is an enum, and its inner data may be one of a set of variants.
    Variants(Vec<Variant>),

    /// The type is a mixin.
    Mixin,
}

/// A variant definition in an enum, when using a [`Type`] with [`TypeData::Variants`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<String>,
}

impl Variant {
    /// Constructs a new variant definition with a named set of fields.
    pub fn new(name: &str, fields: Vec<&str>) -> Self {
        Self {
            name: name.into(),
            fields: fields.into_iter().map(|x| x.into()).collect()
        }
    }

    /// Returns the index of a field within this variant, or `None` if it doesn't exist.
    pub fn field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().enumerate().find(|(_, f)| f == &name).map(|(i, _)| i)
    }
}
