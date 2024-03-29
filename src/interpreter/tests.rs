use std::assert_matches::assert_matches;

use crate::{interpreter::{Interpreter, InterpreterErrorKind, Value}, source::SourceFile};

use super::{ValueRef, InterpreterError};

pub fn evaluate(input: &str) -> Result<ValueRef, InterpreterError> {
    Interpreter::new(Some(SourceFile::new_temp(input).rc()))?.parse_and_evaluate_root()
}

#[test]
fn test_simple_interpret() {
    assert_eq!(
        evaluate("32 add: 24.").unwrap(),
        Value::new_integer(56).rc(),
    );
}

#[test]
fn test_message_precedence() {
    assert_eq!(
        evaluate("2 negate add: 7 negate.").unwrap(),
        Value::new_integer(-9).rc(),
    );

    assert_eq!(
        evaluate("(5 add: 5) sub: (3 add: 4).").unwrap(),
        Value::new_integer(3).rc(),
    );

    assert_eq!(
        evaluate("5 + 2 negate.").unwrap(),
        Value::new_integer(3).rc(),
    );

    assert_eq!(
        evaluate("5 mul: 3 + 2").unwrap(),
        Value::new_integer(25).rc(),
    );
}

#[test]
fn test_sequence() {
    // A sequence evaluates to its last item
    assert_eq!(
        evaluate("1. 2. 3.").unwrap(),
        Value::new_integer(3).rc(),
    )
}

#[test]
fn test_assignment() {
    assert_eq!(
        evaluate("a = 3. b = 4. a add: b.").unwrap(),
        Value::new_integer(7).rc(),
    );

    // Assignment performs a value copy
    assert_eq!(
        evaluate("a = 3. b = a. a = 5. b").unwrap(),
        Value::new_integer(3).rc(),
    );
}

#[test]
fn test_static_methods() {
    assert_eq!(
        evaluate("Integer zero.").unwrap(),
        Value::new_integer(0).rc(),
    )
}

#[test]
fn test_unary_block() {
    assert_eq!(
        evaluate("[ 1 add: 1 ] call.").unwrap(),
        Value::new_integer(2).rc(),
    )
}

#[test]
fn test_param_block() {
    assert_eq!(
        evaluate("[ | a b | a add: b ] callWith: #{ 3 4 }").unwrap(),
        Value::new_integer(7).rc(),
    );

    assert_eq!(
        evaluate("[ | a b | a add: b ] arity").unwrap(),
        Value::new_integer(2).rc(),
    );
}

#[test]
fn test_capture() {
    assert_eq!(
        evaluate("x = [ a = 4. [ | x | a add: x ] ]. (x call) call: 3").unwrap(),
        Value::new_integer(7).rc(),
    );

    assert_eq!(
        evaluate("
            struct Pair first second.
            p = [
                a = 4.
                Pair
                    first: [ a ]
                    second: [ | x | a = a add: x ]
            ] call.
            (p second) call: 2.
            (p second) call: 4.
            (p first) call
        ").unwrap(),
        Value::new_integer(10).rc(),
    );
}

#[test]
fn test_boolean() {
    assert_eq!(
        evaluate("Boolean#True").unwrap(),
        evaluate("true").unwrap(),
    );

    assert_eq!(
        evaluate("Boolean#False").unwrap(),
        evaluate("false").unwrap(),
    );

    assert_eq!(
        evaluate("true not").unwrap(),
        evaluate("false").unwrap(),
    );
}

#[test]
fn test_parameter_assignment() {
    assert_eq!(
        evaluate("
            x = 3.
            [ |i| i = i add: 1 ] call: x.
            x
        ").unwrap(),
        Value::new_integer(3).rc(),
    );
}

#[test]
fn test_enum_definition() {
    let enum_def = "
        enum Occupation {
            Teacher salary yearGroup.
            Doctor salary department.
            Other salary description.
        }
    ";

    // Construction and field access
    assert_eq!(
        evaluate(&format!("
            {enum_def}
            t = Occupation#Teacher salary: 123 yearGroup: 6.
            (t salary) add: (t yearGroup)
        ")).unwrap(),
        Value::new_integer(123 + 6).rc(),
    );

    // Errors on missing/incorrect/extraneous fields
    assert_matches!(
        evaluate(&format!("
            {enum_def}
            Occupation#Teacher salary: 123
        ")),
        Err(InterpreterError { kind: InterpreterErrorKind::IncorrectVariantParameters, .. }),
    );
    assert_matches!(
        evaluate(&format!("
            {enum_def}
            Occupation#Teacher salary: 123 department: \"Health\"
        ")),
        Err(InterpreterError { kind: InterpreterErrorKind::IncorrectVariantParameters, .. }),
    );
    assert_matches!(
        evaluate(&format!("
            {enum_def}
            Occupation#Teacher salary: 123 yearGroup: 6 department: \"Health\"
        ")),
        Err(InterpreterError { kind: InterpreterErrorKind::IncorrectVariantParameters, .. }),
    );

    // Field assignment
    assert_eq!(
        evaluate(&format!("
            {enum_def}
            t = Occupation#Teacher salary: 123 yearGroup: 6.
            t salary = 456.
            t yearGroup = 1.
            (t salary) add: (t yearGroup)
        ")).unwrap(),
        Value::new_integer(456 + 1).rc(),
    );
}

#[test]
fn test_struct_definition() {
    let struct_def = "
        struct Foo a b.
    ";

    // Construction and field access
    assert_eq!(
        evaluate(&format!("
            {struct_def}
            f = Foo a: 123 b: 456
            (f a) add: (f b)
        ")).unwrap(),
        Value::new_integer(123 + 456).rc(),
    );

    // Errors on missing/incorrect/extraneous fields
    assert_matches!(
        evaluate(&format!("
            {struct_def}
            Foo a: 123
        ")),
        Err(InterpreterError { kind: InterpreterErrorKind::MissingMethod(_, _), .. }),
    );
    assert_matches!(
        evaluate(&format!("
            {struct_def}
            Foo a: 123 c: 456
        ")),
        Err(InterpreterError { kind: InterpreterErrorKind::MissingMethod(_, _), .. }),
    );
    assert_matches!(
        evaluate(&format!("
            {struct_def}
            Foo a: 123 b: 456 c: 789
        ")),
        Err(InterpreterError { kind: InterpreterErrorKind::MissingMethod(_, _), .. }),
    );

    // Field assignment
    assert_eq!(
        evaluate(&format!("
            {struct_def}
            f = Foo a: 123 b: 456.
            f a = 789.
            f b = 1.
            (f a) add: (f b)
        ")).unwrap(),
        Value::new_integer(789 + 1).rc(),
    );

    // Blank structs
    assert_eq!(
        evaluate("
            struct Blank.
            impl Blank {
                static func something {
                    3
                }
            }

            Blank something
        ").unwrap(),
        Value::new_integer(3).rc(),
    );
}

#[test]
fn test_impl_block() {
    assert_eq!(
        evaluate("
            struct NumberPair a b.

            impl NumberPair {
                func sum {
                    self a add: self b.
                }

                func combine: other {
                    NumberPair a: self sum b: other
                }
            }

            p = NumberPair a: 2 b: 4.
            (p combine: 10) sum
        ").unwrap(),
        Value::new_integer(16).rc(),
    );

    // Static methods, plus a relatively complex case testing some other stuff (e.g. captures)
    assert_eq!(
        evaluate("
            struct ValueBox getter setter.
            impl ValueBox {
                static func newContaining: value {
                    ValueBox
                        getter: [ value ]
                        setter: [ |v| value = v ]
                }
            
                func get {
                    self getter call
                }
            
                func set: value {
                    self setter call: value
                }
            }
            
            box = ValueBox newContaining: 10.
            oldValue = box get.
            box set: 3.
            newValue = box get.

            oldValue add: newValue
        ").unwrap(),
        Value::new_integer(13).rc(),
    )
}

#[test]
fn test_mixin() {
    // Normal usage
    assert_eq!(
        evaluate("
            mixin XWrapper.
            impl XWrapper {
                func xSucc {
                    self x add: 1
                }
            }

            struct Foo x.
            impl Foo {
                use XWrapper.
            }

            (Foo x: 4) xSucc
        ").unwrap(),
        Value::new_integer(5).rc(),
    );

    // Precedence - methods from the type come before methods on the mixin
    assert_eq!(
        evaluate("
            mixin M.
            impl M {
                func x {
                    1
                }
            }

            enum X { Var. }
            impl X {
                func x {
                    2
                }
                use M.
            }

            X#Var x
        ").unwrap(),
        Value::new_integer(2).rc(),
    );
}

#[test]
fn test_block_equality() {
    // Blocks are only equal if they are the exact same block
    assert_eq!(
        evaluate("
            x = [].
            y = [].
            x equals: y
        ").unwrap().borrow().to_language_string(),
        "Boolean#False",
    );
    assert_eq!(
        evaluate("
            x = [].
            x equals: x
        ").unwrap().borrow().to_language_string(),
        "Boolean#True",
    );
}

#[test]
fn test_array_string() {
    assert_eq!(
        evaluate("
            struct X a b.
            #{ 1 2 (X a: 1 b: 2) 3 true } repr
        ").unwrap(),
        Value::new_string("#{ 1 2 (X a: 1 b: 2) 3 Boolean#True }").rc(),
    );
    assert_eq!(
        evaluate("
            Array new repr
        ").unwrap(),
        Value::new_string("#{ }").rc(),
    );
}

#[test]
fn test_private_methods() {
    assert_matches!(
        evaluate("
            struct Foo a.
            impl Foo {
                private func x {
                    1
                }
            }

            Foo a: 3 $ x
        "),
        Err(InterpreterError { kind: InterpreterErrorKind::PrivateMethod(_, _), .. }),
    );

    assert_eq!(
        evaluate("
            struct Foo a.
            impl Foo {
                private func x {
                    1
                }

                func y {
                    self x
                }
            }

            Foo a: 3 $ y
        ").unwrap(),
        Value::new_integer(1).rc(),
    )
}

#[test]
fn test_static_private_methods() {
    assert_matches!(
        evaluate("
            struct Foo a.
            impl Foo {
                private static func x {
                    1
                }
            }

            Foo x
        "),
        Err(InterpreterError { kind: InterpreterErrorKind::PrivateMethod(_, _), .. }),
    );

    assert_eq!(
        evaluate("
            struct Foo a.
            impl Foo {
                private static func x {
                    1
                }

                static func y {
                    self x
                }
            }

            Foo y
        ").unwrap(),
        Value::new_integer(1).rc(),
    )
}

#[test]
fn test_unordered_methods() {
    // By default, methods are ordered
    assert_matches!(
        evaluate("
            struct Maths.
            impl Maths {
                static func add: x to: y {
                    x + y
                }
            }

            Maths to: 4 add: 5
        "),
        Err(_),
    );

    // Unordered methods can be called in any order
    assert_eq!(
        evaluate("
            struct Maths.
            impl Maths {
                static unordered func add: x to: y {
                    x + y
                }
            }

            (Maths add: 2 to: 3) * (Maths to: 4 add: 5)
        ").unwrap(),
        Value::new_integer((2 + 3) * (4 + 5)).rc(),
    );

    // Constructors are generated as unordered
    assert_eq!(
        evaluate("
            struct Foo x y z.
            f = Foo y: 2 x: 1 z: 3.
            f x
        ").unwrap(),
        Value::new_integer(1).rc(),
    );
}
