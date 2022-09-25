use std::{rc::Rc, assert_matches::assert_matches};

use crate::{parser::Parser, tokenizer::Tokenizer, interpreter::{TypeInstance, Interpreter, InterpreterError, Value}};

use super::{ValueRef, Type, InternalMethod, TypeData};

fn evaluate(input: &str) -> Result<ValueRef, InterpreterError> {
    let node = Parser::parse_and_analyse(&Tokenizer::tokenize(input).unwrap()[..]).unwrap();
    let mut interpreter = Interpreter::new();
    interpreter.evaluate(&node)
}

#[test]
fn test_simple_interpret() {
    assert_eq!(
        evaluate("32 add: 24.").unwrap(),
        Value::new_integer(56).rc(),
    );
}

#[test]
fn test_integer_overflow_on_token_conversion() {
    assert!(matches!(
        //   One more than the maximum i64
        //        vvvvvvvvvvvvvvvvvvv
        evaluate("9223372036854775808."),
        Err(InterpreterError::IntegerOverflow(..)),
    ));
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
        evaluate("[ | a b | a add: b ] call: 3 call: 4").unwrap(),
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
        Err(InterpreterError::IncorrectVariantParameters),
    );
    assert_matches!(
        evaluate(&format!("
            {enum_def}
            Occupation#Teacher salary: 123 department: \"Health\"
        ")),
        Err(InterpreterError::IncorrectVariantParameters),
    );
    assert_matches!(
        evaluate(&format!("
            {enum_def}
            Occupation#Teacher salary: 123 yearGroup: 6 department: \"Health\"
        ")),
        Err(InterpreterError::IncorrectVariantParameters),
    );
}

#[test]
fn test_struct_definition() {
    let enum_def = "
        struct Foo a b.
    ";

    // Construction and field access
    assert_eq!(
        evaluate(&format!("
            {enum_def}
            f = Foo a: 123 b: 456
            (f a) add: (f b)
        ")).unwrap(),
        Value::new_integer(123 + 456).rc(),
    );

    // Errors on missing/incorrect/extraneous fields
    assert_matches!(
        evaluate(&format!("
            {enum_def}
            Foo a: 123
        ")),
        Err(InterpreterError::MissingMethod(_, _)),
    );
    assert_matches!(
        evaluate(&format!("
            {enum_def}
            Foo a: 123 c: 456
        ")),
        Err(InterpreterError::MissingMethod(_, _)),
    );
    assert_matches!(
        evaluate(&format!("
            {enum_def}
            Foo a: 123 b: 456 c: 789
        ")),
        Err(InterpreterError::MissingMethod(_, _)),
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
}
