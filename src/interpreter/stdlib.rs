use crate::{interpreter::{Type, Method, Value}, parser::{SendMessageComponents, SendMessageParameter}};

use super::{InterpreterError, TypeData, Variant, TypeRef, Interpreter};

pub fn instantiate(interpreter: &mut Interpreter) {
    interpreter.types.extend(core_types());
    interpreter.parse_and_evaluate(include_str!("../../stdlib/boolean.bbl")).unwrap();
    interpreter.parse_and_evaluate(include_str!("../../stdlib/integer.bbl")).unwrap();
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
            Method::new_internal("add:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a + b).rc())
            }).rc(),
            Method::new_internal("sub:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a - b).rc())
            }).rc(),
            Method::new_internal("negate", |_, recv, _| {
                let a = recv.borrow().to_integer()?;
                Ok(Value::new_integer(-a).rc())
            }).rc(),
            Method::new_internal("modulo:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a % b).rc())
            }).rc(),

            // TODO: maybe these should be shared?
            Method::new_internal("greaterThan:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a > b).rc())
            }).rc(),
            Method::new_internal("greaterThanOrEquals:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a >= b).rc())
            }).rc(),
            Method::new_internal("lessThan:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a < b).rc())
            }).rc(),
            Method::new_internal("lessThanOrEquals:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a <= b).rc())
            }).rc(),
            Method::new_internal("equals:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a == b).rc())
            }).rc(),
        ],

        static_methods: vec![
            Method::new_internal("zero", |_, _, _| {
                Ok(Value::new_integer(0).rc())
            }).rc(),
        ],

        ..Type::new("Integer")
    }
}

fn string() -> Type {
    Type {
        methods: vec![
            Method::new_internal("concat:", |_, recv, params| {
                let a = recv.borrow().to_string()?;
                let b = params[0].borrow().to_string()?;
                Ok(Value::new_string(&format!("{}{}", a, b)).rc())
            }).rc(),

            // TODO: should be shared
            Method::new_internal("equals:", |i, recv, params| {
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
            Method::new_internal("println:", |_, _, p| {
                println!("{}", p[0].borrow().to_language_string());
                Ok(Value::new_null().rc())
            }).rc(),

            Method::new_internal("print:", |_, _, p| {
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
            Method::new_internal(&method_name, |i, r, a| {
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
        Method::new_internal("arity", |_, r, _| {
            Ok(Value::new_integer(r.borrow().to_block()?.arity() as i64).rc())
        }).rc(),

        Method::new_internal("whileTrue:", |i, r, a| {
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
            Method::new_internal("not", |i, r, _| {
                Ok(Value::new_boolean(i, !r.borrow().to_boolean()?).rc())
            }).rc(),

            Method::new_internal("ifTrue:", |i, r, a| {
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
            Method::new_internal("case:that:equals:", |i, _, a| {
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
