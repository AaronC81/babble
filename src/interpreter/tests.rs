use crate::{parser::Parser, tokenizer::Tokenizer, interpreter::{TypeInstance, Interpreter, InterpreterError, Value}};

use super::ValueRef;

fn evaluate(input: &str) -> Result<ValueRef, InterpreterError> {
    let node = Parser::parse(&Tokenizer::tokenize(input).unwrap()[..]).unwrap();
    Interpreter::new().evaluate(&node)
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
