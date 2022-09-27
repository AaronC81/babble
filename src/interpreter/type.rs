use std::{rc::Rc, fmt::Debug, cell::RefCell};

use crate::interpreter::TypeInstance;

use super::{InterpreterError, Value, Method, MethodRef};

#[derive(Debug)]
pub struct Type {
    pub id: String,
    pub data: TypeData,
    pub methods: Vec<MethodRef>,
    pub static_methods: Vec<MethodRef>,
}
impl PartialEq for Type { fn eq(&self, other: &Self) -> bool { self.id == other.id } }
impl Eq for Type {}

impl Type {
    pub fn new(id: &str) -> Self {
        Self {
            id: id.into(),
            data: TypeData::Empty,
            methods: vec![],
            static_methods: vec![],
        }
    }

    pub fn resolve_method(&self, name: &str) -> Option<MethodRef> {
        self.methods.iter().find(|m| m.name == name).cloned()
    }

    pub fn resolve_static_method(&self, name: &str) -> Option<MethodRef> {
        self.static_methods.iter().find(|m| m.name == name).cloned()
    }

    pub fn resolve_variant(&self, name: &str) -> Result<(usize, &Variant), InterpreterError> {
        let TypeData::Variants(variants) = &self.data else {
            return Err(InterpreterError::VariantAccessOnNonEnum);
        };

        if let Some(variant) = variants.iter().enumerate().find(|(_, v)| v.name == name) {
            Ok(variant)
        } else {
            Err(InterpreterError::MissingVariant(name.into()))
        }
    }

    pub fn add_method(&mut self, method: MethodRef) {
        self.methods.retain(|m| m.name != method.name);
        self.methods.push(method);
    }

    pub fn add_static_method(&mut self, method: MethodRef) {
        self.static_methods.retain(|m| m.name != method.name);
        self.static_methods.push(method);
    }

    pub fn generate_accessor_methods(&mut self) {
        // TODO: setters?

        match &self.data {
            TypeData::Fields(fields) => {
                for (i, field) in fields.clone().iter().enumerate() {
                    self.add_method(Method::new_internal(field, move |_, r, _| {
                        let TypeInstance::Fields { field_values, .. } = &(*r).borrow().type_instance else {
                            return Err(InterpreterError::IncorrectType);
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
                            return Err(InterpreterError::IncorrectType);
                        };
                        let variant = &variants_copy[variant.unwrap()];

                        if let Some(index) = variant.field_index(&field) {
                            Ok(field_values[index].clone())
                        } else {
                            Err(InterpreterError::MissingMethod(r.clone(), field.clone()))
                        }
                    }).rc());
                }
            }

            TypeData::Empty => (),
        }
    }

    pub fn generate_struct_constructor(t: Rc<RefCell<Type>>) {
        // TODO: how to handle empty structs?

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

    pub fn rc(self) -> TypeRef {
        Rc::new(RefCell::new(self))
    }
}

pub type TypeRef = Rc<RefCell<Type>>;

#[derive(Debug, Clone)]
pub enum TypeData {
    Empty,
    Fields(Vec<String>),
    Variants(Vec<Variant>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<String>,
}

impl Variant {
    pub fn new(name: &str, fields: Vec<&str>) -> Self {
        Self {
            name: name.into(),
            fields: fields.into_iter().map(|x| x.into()).collect()
        }
    }

    pub fn field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().enumerate().find(|(_, f)| f == &name).map(|(i, _)| i)
    }
}
