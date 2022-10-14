use crate::source::SourceFile;

use super::{Type, Method, Value, Interpreter};

pub fn derive_core_mixins(interpreter: &mut Interpreter, target: &mut Type) {
    derive_equatable(interpreter, target);
}

pub fn derive_equatable(interpreter: &mut Interpreter, target: &mut Type) {
    target.add_method(Method::new_internal("equals:", |i, recv, params| {
        let this = recv.borrow();
        let other = params[0].borrow();

        Ok(Value::new_boolean(i, this.type_instance == other.type_instance).rc())
    }).rc());
    target.used_mixins.push(interpreter.resolve_stdlib_type("Equatable"));
}

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
