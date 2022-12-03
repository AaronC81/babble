//! Definitions for Babble's standard library.
//! 
//! This Rust module only includes types and methods which must be defined intrinsically, for one
//! reason or another. Where possible, Babble's standard library is defined _in Babble_, with these
//! files imported and executed by [`instantiate`].

use std::{process::exit, sync::RwLock, fs::File, io::{Read, Write}, any::Any};

use crate::{interpreter::{Type, Method, Value}, parser::{SendMessageComponents, SendMessageParameter}, source::SourceFile};

use super::{InterpreterErrorKind, TypeData, Variant, TypeRef, Interpreter, TypeInstance, mixin_derive::TypeCoreMixinDeriveBuilder, InterpreterError, DocumentationState, ValueRef, PrimitiveValue};

macro_rules! load_stdlib_file {
    ($interpreter:expr, $filename:literal) => {
        $interpreter.parse_and_evaluate(SourceFile::new(
            &format!("<stdlib>/{}", $filename),
            include_str!(concat!("../../stdlib/", $filename))
        ).rc())
    }
}

/// Instantiates a set of core standard library types, by building them from intrinsics, executing
/// bundled Babble code to define them, or a combination of the two.
pub fn instantiate(interpreter: &mut Interpreter) -> Result<(), InterpreterError> {
    // Some types are needed to define others, namely derived mixins, so add these to the
    // interpreter in an early pass so they're resolvable
    let early_core_types = early_core_types(interpreter);
    interpreter.types.extend(early_core_types);
    load_stdlib_file!(interpreter, "core_mixins.bbl")?;

    // Define the rest of the stdlib types
    let core_types = core_types(interpreter);
    interpreter.types.extend(core_types);
    load_stdlib_file!(interpreter, "block.bbl")?;
    load_stdlib_file!(interpreter, "program.bbl")?;
    load_stdlib_file!(interpreter, "boolean.bbl")?;
    load_stdlib_file!(interpreter, "integer.bbl")?;
    load_stdlib_file!(interpreter, "array.bbl")?;
    load_stdlib_file!(interpreter, "match.bbl")?;
    load_stdlib_file!(interpreter, "string.bbl")?;
    load_stdlib_file!(interpreter, "range.bbl")?;
    load_stdlib_file!(interpreter, "file.bbl")?;
    load_stdlib_file!(interpreter, "random.bbl")?;
    load_stdlib_file!(interpreter, "dictionary.bbl")?;

    // Run deferred tests
    let deferred_tests = interpreter.resolve_stdlib_type("InternalTest").borrow()
        .static_fields[0].borrow_mut().to_array()?.iter().cloned().collect::<Vec<_>>();
    for test in deferred_tests {
        test.borrow().to_block()?.call(interpreter, vec![])?;
    }

    Ok(())
}

fn early_core_types(interpreter: &mut Interpreter) -> Vec<TypeRef> {
    vec![
        representable(interpreter).rc(),
        equatable(interpreter).rc(),
    ]
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
        file(interpreter).rc(),
    ]
}

fn representable(_: &mut Interpreter) -> Type {
    Type {
        methods: vec![
            Method::new_internal("repr", |_, recv, _| {
                Ok(Value::new_string(&recv.borrow().to_language_string()).rc())
            }).with_documentation("
                A debug string representation of this value.

                @returns The string representation.
            ").rc(),
        ],
        data: TypeData::Mixin,
        ..Type::new("Representable")
    }.with_documentation("\
        Indicates that a type can be converted to a string.

        This mixin is implemented automatically on all types, and the definition of `repr` is
        provided by the interpreter.
    ")
}

fn equatable(_: &mut Interpreter) -> Type {
    Type {
        methods: vec![
            Method::new_internal("equals:", |i, recv, args| {
                let this = recv.borrow();
                let other = args[0].borrow();

                Ok(Value::new_boolean(i, this.type_instance == other.type_instance).rc())
            }).with_documentation("
                Determines whether this object is value-equal to another object.

                @param equals: The other object.
                @returns `true` if the two objects are equal, or `false` otherwise.
            ").rc(),
        ],
        data: TypeData::Mixin,
        ..Type::new("Equatable")
    }.with_documentation("\
        Indicates that values of a type can be compared for equality.

        This mixin is implemented automatically on all types, and the definition of `equals:` is
        provided by the interpreter.
    ")
}

fn null(interpreter: &mut Interpreter) -> Type {
    Type::new("Null")
        .with_derived_core_mixins(interpreter)
        .with_documentation("
            The type of `null`, which represents the absence of a value.
        ")
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
                Subtracts the given integer from this one.

                @param sub: The integer to subtract.
                @returns This integer with the other integer subtracted.
            ").rc(),

            Method::new_internal("mul:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a * b).rc())
            }).with_documentation("
                Multiplies the given integer with this one.

                @param mul: The integer to multiply with.
                @returns The two integers, multiplied.
            ").rc(),

            Method::new_internal("div:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a / b).rc())
            }).with_documentation("
                Performs integer division with another integer.

                @param div: The divisor.
                @returns This integer divided by the other integer.
            ").rc(),

            // TODO: What overflow semantics do we want for these?
            Method::new_internal("leftShift:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a << b).rc())
            }).with_documentation("
                Shifts this integer left by the specified amount.

                @param leftShift: The amount to shift by.
                @returns This integer shifted left by the other integer.
            ").rc(),
            Method::new_internal("rightShift:", |_, recv, params| {
                let a = recv.borrow().to_integer()?;
                let b = params[0].borrow().to_integer()?;
                Ok(Value::new_integer(a >> b).rc())
            }).with_documentation("
                Shifts this integer right by the specified amount.

                @param leftShift: The amount to shift by.
                @returns This integer shifted right by the other integer.
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
    }
    .with_derived_core_mixins(interpreter)
    .with_mixin("Orderable", interpreter)
    .with_documentation("
        A whole number.

        This type is implemented as a 64-bit signed integer.
    ")    
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

            Method::new_internal("toInteger:", |_, recv, a| {
                let base = a[0].borrow().to_integer()?;
                if base < 2 || base > 36 {
                    return Err(InterpreterErrorKind::ProgramError("invalid integer base".into()).into())
                }
                
                Ok(i64::from_str_radix(&recv.borrow().to_string()?, base as u32)
                    .map(Value::new_integer)
                    .unwrap_or_else(|_| Value::new_null())
                    .rc())
            }).with_documentation("
                Parses this string as an integer of the given base. If the string is not a valid integer, returns `null`.

                If the base is outside of the range 2..36 inclusive, raises an error.

                @params toInteger: The base to parse the integer in.
                @returns An integer, or `null`.
            ").rc(),

            Method::new_internal("uppercase", |_, recv, a| {
                Ok(Value::new_string(&recv.borrow().to_string()?.to_uppercase()).rc())
            }).with_documentation("
                Returns a copy of this string, with all letters converted to uppercase.

                @returns The new uppercase string.
            ").rc(),

            Method::new_internal("lowercase", |_, recv, a| {
                Ok(Value::new_string(&recv.borrow().to_string()?.to_lowercase()).rc())
            }).with_documentation("
                Returns a copy of this string, with all letters converted to lowercase.

                @returns The new lowercase string.
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
    }
    .with_derived_core_mixins(interpreter)
    .with_documentation("
        A string of UTF-8 characters.
    ")
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
    }
    .with_derived_core_mixins(interpreter)
    .with_documentation("
        An ordered collection of values. Arrays can have items added or removed from them at will,
        changing their size dynamically.
    ")
}

fn console(_: &mut Interpreter) -> Type {
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
                drop(std::io::stdout().flush());
                Ok(Value::new_null().rc())
            }).with_documentation("
                Prints an object to the console, with no trailing newline.

                Additionally, flushes the console buffer, to ensure that the output is actually
                displayed, since many consoles will not usually show content until a newline is
                printed.

                @param print: The object to print.
            ").rc(),

            Method::new_internal("input", |_, _, _| {
                let line = std::io::stdin().lines().next().unwrap().unwrap();
                Ok(Value::new_string(&line).rc())
            }).with_documentation("
                Reads a line of input from the console.

                @returns The line as a string.
            ").rc(),
        ],

        ..Type::new("Console")
    }
    .with_documentation("
        An interface to standard input and output streams on the host machine.
    ")
}

fn block(interpreter: &mut Interpreter) -> Type {
    Type {
        methods: vec![
            Method::new_internal("callWith:", |i, r, a| {
                let r = r.borrow();
                let b = r.to_block()?;
                b.call(i, a[0].borrow_mut().to_array()?.clone())
            }).with_documentation("
                Calls this block with the given array of arguments.

                @param callWith: The array of arguments to pass to the block.
                @returns The result of the block.
            ").rc(),

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
        ],

        ..Type::new("Block")
    }
    .with_derived_core_mixins(interpreter)
    .with_documentation("
        The type of a block created with `[ ... ]` syntax. Blocks are objects representing snippets
        of code which can be executed with parameters, optionally performing pattern matching on
        those parameters, as well as capturing local variables from their scope.
    ")
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
    }
    .with_derived_core_mixins(interpreter)
    .with_documentation("
        Logical true and false values.
    ")
}

fn internal_test(_: &mut Interpreter) -> Type {
    Type {
        static_methods: vec![
            Method::new_internal("case:that:equals:", |i, _, a| {
                let left = a[1].clone();
                let right = a[2].clone();

                let equal = i.send_message(left, "equals:", vec![right])?;

                if equal.borrow().to_boolean()? {
                    Ok(Value::new_null().rc())
                } else {
                    Err(InterpreterErrorKind::InternalTestFailed(a[0].borrow().to_string()?).into())
                }

            }).rc(),

            Method::new_internal("defer:", |i, r, a| {
                let test = a[0].clone();
                i.resolve_stdlib_type("InternalTest").borrow_mut()
                    .static_fields[0]
                    .borrow_mut()
                    .to_array()?
                    .push(test);
                Ok(Value::new_null().rc())
            }).rc(),
        ],

        static_fields: vec![
            // Deferred tests
            Value::new_array(&[]).rc(),
        ],

        documentation: DocumentationState::Hidden,

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

            Method::new_internal("eval:", |i, _, a| {
                let code = a[0].borrow().to_string()?;
                i.parse_and_evaluate(SourceFile::new("<eval>", &code).rc())
            }).with_documentation("
                Parses, compiles, and executes a string of Babble code.

                As you might guess, this is **extremely dangerous** to use in combination with any
                user input. Be careful!
            ").rc(),

            Method::new_magic("filePathBacktrace").with_documentation("
                An array of absolute file paths, one per stack frame, corresponding to the file from
                which the code executing in that stack frame was loaded.

                The most recent stack frame appears first.

                If any stack frame does not have an associated file, that frame's element will be
                null.

                @magic
                @returns An array of file paths.
            ").rc(),
        ],
        ..Type::new("Program")
    }
    .with_documentation("
        A special type containing static methods for interacting with the interpreter itself.
    ")
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
    .with_documentation("
        A special type containing static methods for retrieving metadata information about values or
        types.
    ")
}

fn file(interpreter: &mut Interpreter) -> Type {
    Type {
        data: TypeData::Empty,

        static_methods: vec![
            Method::new_internal("open:", |_, _, a| {
                let path = a[0].borrow().to_string()?;
                let file = File::open(path.clone())
                    .map_err(|e| InterpreterErrorKind::IoError(e.to_string()).into())?;
                let handle = FileHandle { file: Some(file), path };
                Ok(Value::new_other(handle).rc())
            }).with_documentation("
                Opens a file for reading.

                @param open: The path to the file.
                @returns The opened `File` instance.
            ").rc(),
        ],

        methods: vec![
            Method::new_internal("readAllText", |_, r, _| {
                let mut s = "".into();
                r.borrow().to_other_mut::<FileHandle>()?.file_if_open()?.read_to_string(&mut s)
                    .map_err(|e| InterpreterErrorKind::IoError(e.to_string()).into())?;
                Ok(Value::new_string(&s).rc())
            }).with_documentation("
                Reads the file from the current position until the end, returning its contents.

                @returns The text read from the file.
            ").rc(),

            Method::new_internal("readBytes:", |_, r, a| {
                let mut buf = vec![0; a[0].borrow().to_integer()? as usize];
                let n = r.borrow().to_other_mut::<FileHandle>()?.file_if_open()?
                    .read(&mut buf).map_err(|e| InterpreterErrorKind::IoError(e.to_string()).into())?;
                buf.truncate(n);
                let buf = buf.into_iter().map(|b| Value::new_integer(b as i64).rc()).collect::<Vec<_>>();
                Ok(Value::new_array(&buf).rc())
            }).with_documentation("
                Reads a particular number of bytes from the file, or possibly less (including zero) 
                if the file ends or the bytes are not available.

                @param readBytes: The number of bytes to read.
                @returns An array of bytes read from the file, no longer than the requested number.
            ").rc(),

            Method::new_internal("close", |_, r, _| {
                r.borrow().to_other_mut::<FileHandle>()?.close();
                Ok(Value::new_null().rc())
            }).with_documentation("
                Closes the file. After this, further I/O operations will error.
            ").rc(),
        ],

        ..Type::new("File")
    }
    .with_derived_core_mixins(interpreter)
    .with_documentation("
        Represents a handle to an open file on the host machine's filesystem.
    ")
}

/// Wraps a (possibly closed) file, and its path.
#[derive(Debug)]
struct FileHandle {
    file: Option<File>,
    path: String,
}

impl PrimitiveValue for FileHandle {
    fn get_type(&self, interpreter: &Interpreter) -> TypeRef {
        interpreter.resolve_stdlib_type("File")
    }

    fn to_language_string(&self) -> String {
        format!("File path: \"{}\"", self.path)
    }
}

impl FileHandle {
    /// Gets a mutable reference to the file if it is open, otherwise returns an [InterpreterError]
    /// describing that the file is closed.
    fn file_if_open(&mut self) -> Result<&mut File, InterpreterError> {
        self.file.as_mut().ok_or_else(|| InterpreterErrorKind::IoError("File is closed".into()).into())
    }

    /// Closes the file by dropping the file handle.
    fn close(&mut self) {
        self.file = None;
    }
}
