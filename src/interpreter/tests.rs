use std::rc::Rc;

use crate::{parser::Parser, tokenizer::Tokenizer, interpreter::{TypeInstance, Interpreter, InterpreterError, Value}};

use super::{ValueRef, Type, InternalMethod};

fn evaluate(input: &str) -> Result<ValueRef, InterpreterError> {
    let node = Parser::parse(&Tokenizer::tokenize(input).unwrap()[..]).unwrap();
    let mut interpreter = Interpreter::new();
    interpreter.types.push(Rc::new(Type {
        fields: vec!["first".into(), "second".into()],

        methods: vec![
            InternalMethod::new("first", |_, recv, _| {
                match &recv.borrow().type_instance {
                    TypeInstance::Fields { field_values, .. } => Ok(field_values[0].clone()),
                    _ => unreachable!()
                }
            }).rc(),
            InternalMethod::new("second", |_, recv, _| {
                match &recv.borrow().type_instance {
                    TypeInstance::Fields { field_values, .. } => Ok(field_values[1].clone()),
                    _ => unreachable!()
                }
            }).rc(),
        ],

        static_methods: vec![
            InternalMethod::new("first:second:", |itptr, _, params| {
                Ok(Value {
                    type_instance: TypeInstance::Fields { 
                        source_type: itptr.resolve_type("TestPair").unwrap(),
                        field_values: params,
                    }
                }.rc())
            }).rc()
        ],

        ..Type::new("TestPair")
    }));
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
    )
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
        evaluate("x = [ a = 4. [ | *a x | a add: x ] ]. (x call) call: 3").unwrap(),
        Value::new_integer(7).rc(),
    );

    assert_eq!(
        // Use `TestPair` to have a closure return a "getter" and "setter" to a captured variable,
        // and check that they behave as expected.
        evaluate("
            p = [
                a = 4.
                TestPair
                    first: [ | *a | a ]
                    second: [ | *a x | a = a add: x ]
            ] call.
            (p second) call: 2.
            (p second) call: 4.
            (p first) call
        ").unwrap(),
        Value::new_integer(10).rc(),
    );
}
