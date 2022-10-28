//! Definitions for Babble's standard library.
//! 
//! This Rust module only includes types and methods which must be defined intrinsically, for one
//! reason or another. Where possible, Babble's standard library is defined _in Babble_, with these
//! files imported and executed by [`instantiate`].

use std::process::exit;

use crate::{interpreter::{Type, Method, Value}, parser::{SendMessageComponents, SendMessageParameter}, source::SourceFile};

use super::{InterpreterErrorKind, TypeData, Variant, TypeRef, Interpreter, TypeInstance, mixin_derive::TypeCoreMixinDeriveBuilder, InterpreterError};

/// Instantiates a set of core standard library types, by building them from intrinsics, executing
/// bundled Babble code to define them, or a combination of the two.
pub fn instantiate(interpreter: &mut Interpreter) -> Result<(), InterpreterError> {
    interpreter.parse_and_evaluate(SourceFile::new(
        "<stdlib>/core_mixins.bbl",
        include_str!("../../stdlib/core_mixins.bbl")
    ).rc())?;
    let core_types = core_types(interpreter);
    interpreter.types.extend(core_types);
    interpreter.parse_and_evaluate(SourceFile::new(
        "<stdlib>/boolean.bbl",
        include_str!("../../stdlib/boolean.bbl")
    ).rc())?;
    interpreter.parse_and_evaluate(SourceFile::new(
        "<stdlib>/integer.bbl",
        include_str!("../../stdlib/integer.bbl")
    ).rc())?;
    interpreter.parse_and_evaluate(SourceFile::new(
        "<stdlib>/array.bbl",
        include_str!("../../stdlib/array.bbl")
    ).rc())?;
    interpreter.parse_and_evaluate(SourceFile::new(
        "<stdlib>/string.bbl",
        include_str!("../../stdlib/string.bbl")
    ).rc())?;
    interpreter.parse_and_evaluate(SourceFile::new(
        "<stdlib>/string.bbl",
        include_str!("../../stdlib/program.bbl")
    ).rc())?;

    Ok(())
}

fn core_types(interpreter: &mut Interpreter) -> Vec<TypeRef> {
    vec![
        null(interpreter).rc(),
        integer(interpreter).rc(),
        string(interpreter).rc(),
        array(interpreter).rc(),
        console(interpreter).rc(),
        block(interpreter).rc(),
        boolean(interpreter).rc(),
        internal_test(interpreter).rc(),
        program(interpreter).rc(),
        reflection(interpreter).rc(),
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
            }).with_documentation("
                Adds the given integer to this one.

                @param add: The integer to add.
                @returns The two integers, added.
            ").rc(),

            Method::new_internal("sub:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a - b).rc())
            }).with_documentation("
                Subtracts the given integer to this one.

                @param sub: The integer to subtract.
                @returns The two integers, subtracted.
            ").rc(),

            Method::new_internal("negate", |_, recv, _| {
                let a = recv.borrow().to_integer()?;
                Ok(Value::new_integer(-a).rc())
            }).with_documentation("
                Flips the sign of this integer.

                @returns If this integer is positive, the same integer but negative, and vice versa.
            ").rc(),

            Method::new_internal("modulo:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a % b).rc())
            }).with_documentation("
                Computes the remainder of integer division with a given divisor.

                @param modulo: The divisor.
                @returns The remainder of dividing this integer with the divisor.
            ").rc(),

            // For Orderable implementation
            Method::new_internal("greaterThan:", |i, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_boolean(i, a > b).rc())
            }).with_documentation("
                Returns true if the given integer is strictly larger than this one.

                @param greaterThan: The other integer.
                @returns `true` if the other integer is larger than this one, otherwise `false`.
            ").rc(),
        ],

        static_methods: vec![
            Method::new_internal("zero", |_, _, _| {
                Ok(Value::new_integer(0).rc())
            }).with_documentation("
                The constant integer 0.

                @returns 0.
            ").rc(),
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
            }).with_documentation("
                Appends another string to this one, and returns the concatenated string.

                @param concat: The other string.
                @returns A new string, with the given string appearing after this one.
            ").rc(),

            Method::new_internal("length", |_, recv, _| {
                Ok(Value::new_integer(recv.borrow().to_string()?.len() as i64).rc())
            }).with_documentation("
                The length of this string.

                @returns The number of characters in this string.
            ").rc(),

            Method::new_internal("charAt:", |_, recv, params| {
                let s = recv.borrow().to_string()?;
                let i = params[0].borrow().to_integer()?;
                if let Some(c) = s.chars().nth(i as usize) {
                    Ok(Value::new_string(&c.to_string()).rc())
                } else {
                    Ok(Value::new_null().rc())
                }
            }).with_documentation("
                Gets a single character from this string.

                @param charAt: The index of the character to get.
                @returns A substring of a single character, or `null` if the index is out of bounds.
            ").rc(),
        ],

        static_methods: vec![
            Method::new_internal("charFromAsciiCode:", |_, _, params| {
                let code = params[0].borrow().to_integer()?;
                if code >= 0 && code <= 0xFF {
                    Ok(Value::new_string(&(code as u8 as char).to_string()).rc())
                } else {
                    Ok(Value::new_null().rc())
                }
            }).with_documentation("
                Constructs a new single-character string from an ASCII character code.

                @param charsFromAsciiCode: The character code as an integer.
                @returns A single-character string, or `null` if the code is out of bounds.
            ").rc(),
        ],

        ..Type::new("String")
    }.with_derived_core_mixins(interpreter)
}

fn array(interpreter: &mut Interpreter) -> Type {
    Type {
        methods: vec![
            // TODO: deal with negative indexes

            Method::new_internal("get:", |_, recv, params| {
                let i = params[0].borrow().to_integer()?;
                Ok(
                    recv.borrow_mut().to_array()?
                        .get(i as usize)
                        .map(|x| x.clone())
                        .unwrap_or_else(|| Value::new_null().rc())
                )
            }).with_documentation("
                Gets an item from this array.

                @param get: The zero-indexed index of the item to get.
                @returns The item, or `null` if the index is out of bounds.
            ").rc(),

            Method::new_internal("set:value:", |_, recv, params| {
                let i = params[0].borrow().to_integer()?;

                // Extend the array if it's not long enough
                let mut recv = recv.borrow_mut();
                let array = recv.to_array()?;
                if i as usize >= array.len() {
                    array.resize((i as usize) + 1, Value::new_null().rc());
                }
                array[i as usize] = params[1].clone();

                Ok(Value::new_null().rc())
            }).with_documentation("
                Sets an item in this array. If the given index does not already exist in the array,
                it is extended with `null`s to match the required length.

                @param set: The zero-indexed index of the item to set.
                @param value: The value to write into the array.
            ").rc(),

            Method::new_internal("append:", |_, recv, params| {
                let item = params[0].clone();
                recv.borrow_mut().to_array()?.push(item);
                Ok(Value::new_null().rc())
            }).with_documentation("
                Adds a new item to the end of this array.

                @param append: The new item to add.
            ").rc(),

            Method::new_internal("insert:at:", |_, recv, params| {
                let item = params[0].clone();
                let index = params[1].borrow().to_integer()?;

                // Resize the array if it's not long enough
                let mut recv = recv.borrow_mut();
                let array = recv.to_array()?;
                if index as usize >= array.len() {
                    array.resize(index as usize, Value::new_null().rc());
                }
                array.insert(index as usize, item);

                Ok(Value::new_null().rc())
            }).with_documentation("
                Inserts a new item at a specific index within this array. If the array is not long
                enough, it is extended with `null`s so that the new item is at the end of the array.

                @param insert: The new item to add.
                @param at: The zero-indexed index at which to insert it.
            ").rc(),


            Method::new_internal("delete:", |_, recv, params| {
                let i = params[0].borrow().to_integer()?;

                let mut recv = recv.borrow_mut();
                let array = recv.to_array()?;
                let item;
                if (i as usize) < array.len() {
                    item = array.remove(i as usize);
                } else {
                    item = Value::new_null().rc();
                }
                Ok(item)
            }).with_documentation("
                Deletes an item at a specific index, and shifts the array elements down to
                compensate. If the array does not cover the index, returns `null`.

                @param delete: The zero-indexed index to delete.
                @returns The deleted item, or `null` if there was no item at that index.
            ").rc(),

            Method::new_internal("length", |_, recv, _| {
                Ok(Value::new_integer(recv.borrow_mut().to_array()?.len() as i64).rc())
            }).with_documentation("
                The length of the array.

                @returns The number of items in the array.
            ").rc(),
        ],

        static_methods: vec![
            Method::new_internal("new", |_, _, _| {
                Ok(Value::new_array(&[]).rc())
            }).with_documentation("
                Constructs a new array.

                @returns A new array with length 0.
            ").rc(),
        ],

        ..Type::new("Array")
    }.with_derived_core_mixins(interpreter)
}

fn console(interpreter: &mut Interpreter) -> Type {
    Type {
        static_methods: vec![
            Method::new_internal("println:", |_, _, p| {
                println!("{}", p[0].borrow().to_language_string());
                Ok(Value::new_null().rc())
            }).with_documentation("
                Prints an object to the console, followed by a newline.

                @param println: The object to print.
            ").rc(),

            Method::new_internal("print:", |_, _, p| {
                print!("{}", p[0].borrow().to_language_string());
                Ok(Value::new_null().rc())
            }).with_documentation("
                Prints an object to the console, with no trailing newline.

                @param print: The object to print.
            ").rc(),
        ],

        ..Type::new("Console")
    }
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
        }).with_documentation("
            How many arguments this block takes.

            @returns An integer representing the number of arguments this block takes.
        ").rc(),

        Method::new_internal("whileTrue:", |i, r, a| {
            let r = r.borrow();
            let condition_block = r.to_block()?;

            while condition_block.call(i, vec![])?.borrow().to_boolean()? {
                let arg = a[0].borrow();
                let block = arg.to_block()?;
                block.call(i, vec![])?;
            }

            Ok(Value::new_null().rc())
        }).with_documentation("
            Repeatedly executes another block (the _body_) while this block (the _condition_)
            continues to return `true`. If the condition returns `false` the first time it is
            executed, the body is never executed.

            @param whileTrue: The block to execute as a condition.
        ").rc(),
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
            }).with_documentation("
                Logically negates this boolean.

                @returns `true` if this is `false`, or `false` if this is `true`.
            ").rc(),

            Method::new_internal("ifTrue:", |i, r, a| {
                if r.borrow().to_boolean()? {
                    let arg = a[0].borrow();
                    let block = arg.to_block()?;
                    block.call(i, vec![])?;
                }

                Ok(r)
            }).with_documentation("
                Executes a block if this is `true`.

                @param ifTrue: The block to execute if this is `true`.
                @returns This boolean.
            ").rc(),
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
    }
}

fn program(_: &mut Interpreter) -> Type {
    Type {
        static_methods: vec![
            Method::new_internal("error:", |_, _, a| {
                Err(InterpreterErrorKind::ProgramError(a[0].borrow().to_string()?).into())
            }).with_documentation("
                Exits the interpreter immediately, with a fatal error.

                @param error: The error message to exit with.
                @returns This method will never return.
            ").rc(),

            Method::new_internal("exit", |_, _, _| {
                exit(0)
            }).with_documentation("
                Exits the interpreter immediately, with a success exit code.

                @returns This method will never return.
            ").rc(),

            Method::new_internal("throw:", |_, _, a| {
                Err(InterpreterErrorKind::Throw(a[0].clone()).into())
            }).with_documentation("
                Throws an object up the call stack. This can be used to implement complex control
                flow mechanisms, such as loop breaks.
                
                This must be executed inside a block called by a catch method, such as
                `catchIfTrue:in:`. (There may be additional blocks or method calls between the throw
                and catch.) If the thrown object reaches the top of the call stack and is uncaught,
                the interpreter exits with a fatal error.

                @param throw: The object to throw.
                @returns This method will never return.
            ").rc(),

            Method::new_internal("catchIfTrue:in:", |i, _, a| {
                let predicate = a[0].borrow().to_block()?.clone();
                let block = a[1].borrow().to_block()?.clone();
                let result = block.call(i, vec![]);
                match result {
                    Err(InterpreterError { kind: InterpreterErrorKind::Throw(ref v), .. }) => {
                        if predicate.call(i, vec![v.clone()])?.borrow().to_boolean()? {
                            Ok(v.clone())
                        } else {
                            Err(InterpreterErrorKind::Throw(v.clone()).into())
                        }
                    }

                    Ok(v) => Ok(v),
                    Err(e) => Err(e),
                }
            }).with_documentation("
                Runs the given body block, and exits if an object is thrown in it with `throw:` and
                the predicate block returns `true` for that thrown object. If the predicate block
                returns `false`, the object is rethrown.

                @param catchIfTrue: The predicate.
                @param in: The body to execute.
                @returns The value returned by the block, or the thrown value if the block exits early.
            ").rc(),
        ],
        ..Type::new("Program")
    }
}

fn reflection(_: &mut Interpreter) -> Type {
    Type {
        static_methods: vec![
            Method::new_internal("type:", |i, _, a| {
                Ok(Value::new_type(a[0].borrow().type_instance.get_type(i)).rc())
            }).with_documentation("
                Gets the type of an object.

                @param type: The object whose type to fetch.
                @returns The object's type.
            ").rc(),

            Method::new_internal("variant:", |i, _, a| {
                let val = a[0].borrow();
                let val_type = val.type_instance.get_type(i);
                let TypeData::Variants(variants) = &val_type.borrow().data else {
                    return Ok(Value::new_null().rc())
                };
                let TypeInstance::Fields { variant, .. } = &val.type_instance else {
                    return Ok(Value::new_null().rc())
                };
                if let Some(v) = variant {
                    Ok(Value::new_string(&variants[*v].name).rc())
                } else {
                    Ok(Value::new_null().rc())
                }
            }).with_documentation("
                Gets the name of the variant of an enum instance.

                @param variant: The enum instance.
                @returns The variant's name as a string, or `null` if it is not an enum variant.
            ").rc(),
        ],
        ..Type::new("Reflection")
    }
}
