use std::{rc::Rc, cell::RefCell};

use crate::{interpreter::{Type, InternalMethod, TypeInstance, Value}, parser::{SendMessageComponents, SendMessageParameter}};

use super::{InterpreterError, TypeData, Variant, TypeRef, Interpreter};

pub fn instantiate(interpreter: &mut Interpreter) {
    interpreter.types.extend(core_types());
    interpreter.parse_and_evaluate(include_str!("../../stdlib/boolean.lang")).unwrap();
    interpreter.parse_and_evaluate(include_str!("../../stdlib/integer.lang")).unwrap();
}

fn core_types() -> Vec<TypeRef> {
    vec![
        null().rc(),
        integer().rc(),
        string().rc(),
        console().rc(),
        block().rc(),
        boolean().rc(),
        internal_test().rc(),
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
            InternalMethod::new("modulo:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a % b).rc())
            }).rc(),

            // TODO: maybe these should be shared?
            InternalMethod::new("greaterThan:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a > b).rc())
            }).rc(),
            InternalMethod::new("greaterThanOrEquals:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a >= b).rc())
            }).rc(),
            InternalMethod::new("lessThan:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a < b).rc())
            }).rc(),
            InternalMethod::new("lessThanOrEquals:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a <= b).rc())
            }).rc(),
            InternalMethod::new("equals:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a == b).rc())
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

fn string() -> Type {
    Type {
        methods: vec![
            InternalMethod::new("concat:", |_, recv, params| {
                let a = recv.borrow().to_string()?;
                let b = params[0].borrow().to_string()?;
                Ok(Value::new_string(&format!("{}{}", a, b)).rc())
            }).rc(),

            // TODO: should be shared
            InternalMethod::new("equals:", |i, recv, params| {
                let a = recv.borrow().to_string()?;
                let b = params[0].borrow().to_string()?;
                Ok(Value::new_boolean(i, a == b).rc())
            }).rc(),
        ],

        ..Type::new("String")
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

    methods.extend([
        InternalMethod::new("arity", |_, r, _| {
            Ok(Value::new_integer(r.borrow().to_block()?.arity() as i64).rc())
        }).rc(),

        InternalMethod::new("whileTrue:", |i, r, a| {
            let r = r.borrow();
            let condition_block = r.to_block()?;

            while condition_block.call(i, vec![])?.borrow().to_boolean()? {
                let arg = a[0].borrow();
                let block = arg.to_block()?;
                block.call(i, vec![])?;
            }

            Ok(Value::new_null().rc())
        }).rc(),
    ]);

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
        ],

        ..Type::new("Boolean")
    }
}

fn internal_test() -> Type {
    Type {
        static_methods: vec![
            InternalMethod::new("case:that:equals:", |i, _, a| {
                let left = a[1].clone();
                let right = a[2].clone();

                let equal = i.send_message(left, &SendMessageComponents::Parameterised(vec![
                    ("equals".into(), SendMessageParameter::Evaluated(right))
                ]))?;

                if equal.borrow().to_boolean()? {
                    Ok(Value::new_null().rc())
                } else {
                    Err(InterpreterError::InternalTestFailed(a[0].borrow().to_string()?))
                }

            }).rc(),
        ],

        ..Type::new("InternalTest")
    }
}
