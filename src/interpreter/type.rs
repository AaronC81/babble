use std::{rc::Rc, fmt::Debug, borrow::Borrow, cell::RefCell};

use super::{ValueRef, InterpreterResult, InterpreterError, Interpreter};

#[derive(Debug)]
pub struct Type {
    pub id: String,
    pub data: TypeData,
    pub methods: Vec<InternalMethodRef>,
    pub static_methods: Vec<InternalMethodRef>,
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

    pub fn resolve_method(&self, name: &str) -> Option<InternalMethodRef> {
        self.methods.iter().find(|m| m.name == name).cloned()
    }

    pub fn resolve_static_method(&self, name: &str) -> Option<InternalMethodRef> {
        self.static_methods.iter().find(|m| m.name == name).cloned()
    }

    pub fn resolve_variant(&self, name: &str) -> Result<(usize, &Variant), InterpreterError> {
        let TypeData::Variants(variants) = &self.data else {
            return Err(InterpreterError::VariantAccessOnNonEnum);
        };

        if let Some(variant) = variants.iter().enumerate().find(|(_, v)| &v.name == name) {
            Ok(variant)
        } else {
            Err(InterpreterError::MissingVariant(name.into()))
        }
    }

    pub fn add_method(&mut self, method: InternalMethodRef) {
        self.methods.retain(|m| m.name != method.name);
        self.methods.push(method);
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

#[derive(Debug, Clone)]
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
}

pub struct InternalMethod {
    pub name: String,
    function: Box<dyn Fn(&mut Interpreter, ValueRef, Vec<ValueRef>) -> InterpreterResult>,
}
impl Debug for InternalMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InternalMethod").field("name", &self.name).finish()
    }
}

pub type InternalMethodRef = Rc<InternalMethod>;

impl InternalMethod {
    pub fn new<F>(name: &str, function: F) -> Self
    where F: Fn(&mut Interpreter, ValueRef, Vec<ValueRef>) -> InterpreterResult + 'static {
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

    pub fn call(&self, interpreter: &mut Interpreter, receiver: ValueRef, parameters: Vec<ValueRef>) -> InterpreterResult {        
        if self.arity() != parameters.len() {
            return Err(InterpreterError::IncorrectArity {
                name: self.name.clone(),
                expected: self.arity(),
                got: parameters.len(),
            })
        }

        (self.function)(interpreter, receiver, parameters)
    }
}
