//! Babble has a set of "core mixins", which every type should implement for convenience.
//! 
//! This module provides [`derive_core_mixins`], which automatically defines instrinsic methods
//! and then `use`-s each core mixin, to provide a working definition.

use crate::source::SourceFile;

use super::{Type, Method, Value, Interpreter};

/// Provides core mixins for a given type.
pub fn derive_core_mixins(interpreter: &mut Interpreter, target: &mut Type) {
    target.used_mixins.push(interpreter.resolve_stdlib_type("Equatable"));
    target.used_mixins.push(interpreter.resolve_stdlib_type("Representable"));
}

/// An extension trait for `Type` which allows for easy derivation of core mixins.
pub trait TypeCoreMixinDeriveBuilder where Self: Sized {
    fn get_self_as_type(self) -> Type;

    fn with_derived_core_mixins(self, interpreter: &mut Interpreter) -> Type {
        let mut t = self.get_self_as_type();
        derive_core_mixins(interpreter, &mut t);
        t
    }

    fn with_mixin(self, name: &str, interpreter: &mut Interpreter) -> Type {
        let mut t = self.get_self_as_type();
        t.used_mixins.push(interpreter.resolve_stdlib_type(name));
        t
    }
}

impl TypeCoreMixinDeriveBuilder for Type {
    fn get_self_as_type(self) -> Type { self }
}

#[test]
fn test_derive_equatable_struct() {
    let mut i = Interpreter::new().unwrap();
    i.parse_and_evaluate(SourceFile::new_temp("struct A x. struct B x.").rc()).unwrap();

    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A x: 3) equals: (A x: 3)").rc()).unwrap(),
        Value::new_boolean(&i, true).rc(),
    );
    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A x: 3) equals: (A x: 4)").rc()).unwrap(),
        Value::new_boolean(&i, false).rc(),
    );
    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A x: 3) notEquals: (A x: 4)").rc()).unwrap(),
        Value::new_boolean(&i, true).rc(),
    );

    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A x: 3) equals: (B x: 3)").rc()).unwrap(),
        Value::new_boolean(&i, false).rc(),
    );
}

#[test]
fn test_derive_equatable_enum() {
    let mut i = Interpreter::new().unwrap();
    i.parse_and_evaluate(SourceFile::new_temp("enum A { X x. Y. } enum B { X x. Y. }").rc()).unwrap();

    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A#Y) equals: (A#Y)").rc()).unwrap(),
        Value::new_boolean(&i, true).rc(),
    );
    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A#X x: 3) equals: (A#X x: 3)").rc()).unwrap(),
        Value::new_boolean(&i, true).rc(),
    );
    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A#X x: 3) equals: (A#X x: 4)").rc()).unwrap(),
        Value::new_boolean(&i, false).rc(),
    );
    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A#X x: 3) notEquals: (A#X x: 4)").rc()).unwrap(),
        Value::new_boolean(&i, true).rc(),
    );

    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A#Y) equals: (B#Y)").rc()).unwrap(),
        Value::new_boolean(&i, false).rc(),
    );
    assert_eq!(
        i.parse_and_evaluate(SourceFile::new_temp("(A#X x: 3) equals: (B#X x: 3)").rc()).unwrap(),
        Value::new_boolean(&i, false).rc(),
    );
}
