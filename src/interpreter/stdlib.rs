use std::{rc::Rc, cell::RefCell};

use crate::{interpreter::{Type, InternalMethod, TypeInstance, Value}, parser::SendMessageComponents};

pub fn types() -> Vec<Rc<Type>> {
    vec![
        Rc::new(null()),
        Rc::new(integer()),
        Rc::new(console()),
        Rc::new(block0()),
    ]
}

fn null() -> Type {
    Type::new("Null")
}

fn integer() -> Type {
    Type {
        methods: vec![
            InternalMethod::new("add:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a + b).rc())
            }).rc(),
            InternalMethod::new("sub:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a - b).rc())
            }).rc(),
            InternalMethod::new("negate", |_, recv, _| {
                let a = recv.borrow().to_integer()?;
                Ok(Value::new_integer(-a).rc())
            }).rc(),

            InternalMethod::new("times:", |i, recv, params| {
                let times = recv.borrow().to_integer()?;
                let block = &params[0];
                for _ in 0..times {
                    i.send_message(block.clone(), &SendMessageComponents::Unary("call".into()))?;
                }

                Ok(recv)
            }).rc(),
        ],

        static_methods: vec![
            InternalMethod::new("zero", |_, _, _| {
                Ok(Value::new_integer(0).rc())
            }).rc(),
        ],

        ..Type::new("Integer")
    }
}

fn console() -> Type {
    Type {
        static_methods: vec![
            InternalMethod::new("println:", |_, _, p| {
                println!("{}", p[0].borrow().to_language_string());
                Ok(Value::new_null().rc())
            }).rc(),

            InternalMethod::new("print:", |_, _, p| {
                print!("{}", p[0].borrow().to_language_string());
                Ok(Value::new_null().rc())
            }).rc(),
        ],

        ..Type::new("Console")
    }
}

// TODO: we can auto-gen and memoize this type for every block of different arities
fn block0() -> Type {
    Type {
        methods: vec![
            InternalMethod::new("call", |i, r, a| {
                match &r.borrow().type_instance {
                    TypeInstance::Block(b) => {
                        b.call(i, a)
                    }
                    _ => unreachable!()
                }
            }).rc(),
        ],

        ..Type::new("Block0")
    }
}
