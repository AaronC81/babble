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
                InternalMethod {
                    name: "add:".into(),
                    function: Box::new(|recv, params| {
                        // TODO: function for this
                        if params.len() != 1 { panic!("incorrect number of parameters") }
                        if let TypeInstance::PrimitiveInteger(a) = recv.borrow().type_instance {
                            if let TypeInstance::PrimitiveInteger(b) = params[0].borrow().type_instance {
                                Ok(Value::new_integer(a + b).rc())
                            } else {
                                // TODO: soft Err
                                panic!("parameter type incorrect")
                            }
                        } else {
                            // TODO: function for this
                            panic!("receiver has incorrect type")   
                        }
                    })
                }
            ],
        }
    }
}
