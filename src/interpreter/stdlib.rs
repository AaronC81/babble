use crate::{interpreter::{Type, Method, Value}, parser::{SendMessageComponents, SendMessageParameter}, source::SourceFile};

use super::{InterpreterErrorKind, TypeData, Variant, TypeRef, Interpreter, mixin_derive::TypeCoreMixinDeriveBuilder};

pub fn instantiate(interpreter: &mut Interpreter) {
    interpreter.parse_and_evaluate(SourceFile::new(
        "<stdlib>/core_mixins.bbl",
        include_str!("../../stdlib/core_mixins.bbl")
    ).rc()).unwrap();
    let core_types = core_types(interpreter);
    interpreter.types.extend(core_types);
    interpreter.parse_and_evaluate(SourceFile::new(
        "<stdlib>/boolean.bbl",
        include_str!("../../stdlib/boolean.bbl")
    ).rc()).unwrap();
    interpreter.parse_and_evaluate(SourceFile::new(
        "<stdlib>/integer.bbl",
        include_str!("../../stdlib/integer.bbl")
    ).rc()).unwrap();
}

fn core_types(interpreter: &mut Interpreter) -> Vec<TypeRef> {
    vec![
        null(interpreter).rc(),
        integer(interpreter).rc(),
        string(interpreter).rc(),
        console(interpreter).rc(),
        block(interpreter).rc(),
        boolean(interpreter).rc(),
        internal_test(interpreter).rc(),
        program(interpreter).rc(),
    ]
}

fn null(interpreter: &mut Interpreter) -> Type {
    Type::new("Null").with_derived_core_mixins(interpreter)
}

fn integer(interpreter: &mut Interpreter) -> Type {
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

            // For Orderable implementation
            Method::new_internal("greaterThan:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a > b).rc())
            }).rc(),
        ],

        static_methods: vec![
            Method::new_internal("zero", |_, _, _| {
                Ok(Value::new_integer(0).rc())
            }).rc(),
        ],

        ..Type::new("Integer")
    }.with_derived_core_mixins(interpreter).with_mixin("Orderable", interpreter)
}

fn string(interpreter: &mut Interpreter) -> Type {
    Type {
        methods: vec![
            Method::new_internal("concat:", |_, recv, params| {
                let a = recv.borrow().to_string()?;
                let b = params[0].borrow().to_string()?;
                Ok(Value::new_string(&format!("{}{}", a, b)).rc())
            }).rc(),

            Method::new_internal("length", |_, recv, _| {
                Ok(Value::new_integer(recv.borrow().to_string()?.len() as i64).rc())
            }).rc(),

            Method::new_internal("charAt:", |_, recv, params| {
                let s = recv.borrow().to_string()?;
                let i = params[0].borrow().to_integer()?;
                if let Some(c) = s.chars().nth(i as usize) {
                    Ok(Value::new_string(&c.to_string()).rc())
                } else {
                    Ok(Value::new_null().rc())
                }
            }).rc(),

            // TODO: should be shared
            Method::new_internal("equals:", |i, recv, params| {
                let a = recv.borrow().to_string()?;
                let b = params[0].borrow().to_string()?;
                Ok(Value::new_boolean(i, a == b).rc())
            }).rc(),
        ],

        ..Type::new("String")
    }.with_derived_core_mixins(interpreter)
}

fn console(interpreter: &mut Interpreter) -> Type {
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
    }.with_derived_core_mixins(interpreter)
}

pub const MAXIMUM_BLOCK_ARITY: usize = 64;

fn block(interpreter: &mut Interpreter) -> Type {
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
                    Err(InterpreterErrorKind::IncorrectBlockArity {
                        expected: b.arity(),
                        got: a.len(),
                    }.into())
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
    }.with_derived_core_mixins(interpreter)
}

fn boolean(interpreter: &mut Interpreter) -> Type {
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
    }.with_derived_core_mixins(interpreter)
}

fn internal_test(interpreter: &mut Interpreter) -> Type {
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
                    Err(InterpreterErrorKind::InternalTestFailed(a[0].borrow().to_string()?).into())
                }

            }).rc(),
        ],

        ..Type::new("InternalTest")
    }.with_derived_core_mixins(interpreter)
}

fn program(_: &mut Interpreter) -> Type {
    Type {
        static_methods: vec![
            Method::new_internal("error:", |_, _, a| {
                Err(InterpreterErrorKind::ProgramError(a[0].borrow().to_string()?).into())
            }).rc(),
        ],
        ..Type::new("Program")
    }
}
