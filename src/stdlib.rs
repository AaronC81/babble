use std::{rc::Rc, cell::RefCell};

use crate::interpreter::{Type, InternalMethod, TypeInstance, Value};

pub struct StandardLibrary {
    pub integer: Rc<Type>,
}

impl StandardLibrary {
    pub fn new() -> Self {
        Self { integer: Rc::new(Self::integer()) }
    }

    fn integer() -> Type {
        Type {
            id: "Integer".into(),
            fields: vec![],
            methods: vec![
                InternalMethod::new("add:", |recv, params| {
                    let a = recv.borrow().to_integer()?;
                    let b = params[0].borrow().to_integer()?;
                    Ok(Value::new_integer(a + b).rc())
                })
            ],
        }
    }
}
