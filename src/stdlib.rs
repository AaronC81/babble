use std::{rc::Rc, cell::RefCell};

use crate::interpreter::{Type, InternalMethod, TypeInstance, Value};

pub fn types() -> Vec<Rc<Type>> {
    vec![
        Rc::new(null()),
        Rc::new(integer()),
    ]
}

fn null() -> Type {
    Type::new("Null")
}

fn integer() -> Type {
    Type {
        methods: vec![
            InternalMethod::new("add:", |recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a + b).rc())
            }).rc(),
            InternalMethod::new("negate", |recv, _| {
                let a = recv.borrow().to_integer()?;
                Ok(Value::new_integer(-a).rc())
            }).rc(),
        ],

        static_methods: vec![
            InternalMethod::new("zero", |_, _| {
                Ok(Value::new_integer(0).rc())
            }).rc(),
        ],

        ..Type::new("Integer")
    }
}
