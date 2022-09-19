use std::{rc::Rc, cell::RefCell};

use crate::{interpreter::{Type, InternalMethod, TypeInstance, Value}, parser::SendMessageComponents};

use super::{InterpreterError, TypeData, Variant};

pub fn types() -> Vec<Rc<Type>> {
    vec![
        Rc::new(null()),
        Rc::new(integer()),
        Rc::new(console()),
        Rc::new(block()),
        Rc::new(boolean()),
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

pub const MAXIMUM_BLOCK_ARITY: usize = 64;

fn block() -> Type {
    let mut methods = vec![];
    
    for i in 0..=MAXIMUM_BLOCK_ARITY {
        let method_name = if i == 0 {
            "call".into()
        } else {
            "call:".repeat(i)
        };

        methods.push(
            InternalMethod::new(&method_name, |i, r, a| {
                let r = r.borrow();
                let b = r.to_block()?;
                if b.arity() != a.len() {
                    Err(InterpreterError::IncorrectBlockArity {
                        expected: b.arity(),
                        got: a.len(),
                    })
                } else {
                    b.call(i, a)
                }
            }).rc(),
        );
    }

    methods.push(InternalMethod::new("arity", |_, r, _| {
        Ok(Value::new_integer(r.borrow().to_block()?.arity() as i64).rc())
    }).rc());

    Type {
        methods,
        ..Type::new("Block")
    }
}

fn boolean() -> Type {
    Type {
        data: TypeData::Variants(vec![
            Variant::new("False", vec![]),
            Variant::new("True", vec![]),
        ]),

        methods: vec![
            InternalMethod::new("not", |i, r, _| {
                Ok(Value::new_boolean(i, !r.borrow().to_boolean()?).rc())
            }).rc(),

            InternalMethod::new("ifTrue:", |i, r, a| {
                if r.borrow().to_boolean()? {
                    let arg = a[0].borrow();
                    let block = arg.to_block()?;
                    block.call(i, vec![])?;
                }

                Ok(r)
            }).rc(),

            InternalMethod::new("ifFalse:", |i, r, a| {
                if !r.borrow().to_boolean()? {
                    let arg = a[0].borrow();
                    let block = arg.to_block()?;
                    block.call(i, vec![])?;
                }

                Ok(r)
            }).rc(),
        ],

        ..Type::new("Boolean")
    }
}
