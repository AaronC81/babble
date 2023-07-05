use std::collections::HashMap;

use crate::parser::{Pattern, PatternKind};

use super::{ValueRef, InterpreterError, InterpreterErrorKind, Value, TypeInstance, TypeData, Interpreter};

/// Matches the given value against this pattern, returning a boolean indicating whether the 
/// pattern matched.
/// 
/// If the pattern contains bindings, the bound values are recorded in the provided `context`.
pub fn match_against(pattern: &Pattern, value: ValueRef, context: &mut PatternMatchContext) -> Result<bool, InterpreterError> {
    match &pattern.kind {
        PatternKind::Literal(expected) => {
            let expected_value = expected.instantiate(context.interpreter)?;
            Ok(
                context.interpreter.send_message(value, "equals:", &[expected_value])?
                    .borrow().to_boolean().unwrap()
            )
        },

        PatternKind::Array(patterns) => {
            // Extract array
            let mut value = value.borrow_mut();
            let array = value.as_array();
            let Ok(array) = array else { return Ok(false) };

            // Trying matching against each pattern
            if patterns.len() != array.len() {
                return Ok(false);
            }
            for (pattern, value) in patterns.iter().zip(array.iter()) {
                if !match_against(pattern, value.clone(), context)? {
                    return Ok(false);
                }
            }

            Ok(true)
        },

        PatternKind::Fields { type_name, variant_name, fields } => {
            // Resolve the type being matched against
            let type_value = if let Some(type_name) = type_name {
                // If we know the type name, then this is easy!
                let Some(type_value) = context.interpreter.resolve_type(type_name) else {
                    return Err(InterpreterErrorKind::MissingName(type_name.into()).into())
                };
                type_value
            } else {
                // If not, then shorthand enum variant syntax was used
                // We need to resolve the type from the captured `self` in the context, using
                // the same method used by desugaring (`Reflection instanceType:`)
                let reflection = Value::new_type(
                    context.interpreter.resolve_stdlib_type("Reflection")
                ).rc();
                let type_value = context.interpreter.send_message(
                    reflection,
                    "instanceType:",
                    &[context.captured_self.clone()],
                )?;
                let type_value = type_value.borrow().to_type()?;
                type_value
            };

            // If there's also a variant involved, resolve that too
            let type_value_borrow = type_value.borrow();
            let variant_value = variant_name
                .as_ref()
                .map(|v| type_value_borrow.resolve_variant(v))
                .transpose()?;
            let variant_index = variant_value.map(|(i, _)| i);
            drop(type_value_borrow);

            // Check pattern type against actual value's type
            if value.borrow().type_instance.get_type(context.interpreter) != type_value {
                return Ok(false)
            }
            
            // If fields were given, or we were given a variant, then assert that the value's
            // data is fields
            if !fields.is_empty() || variant_index.is_some() {
                let TypeInstance::Fields {
                    variant: actual_variant,
                    field_values: actual_field_values,
                    ..
                } = &value.borrow().type_instance else {
                    return Ok(false)
                };

                // Check variant
                if variant_index != *actual_variant {
                    return Ok(false)
                }
                
                // Check fields
                for (field_name, pattern) in fields {
                    // Resolve the field index
                    let field_names = match type_value.borrow().data {
                        TypeData::Fields { ref instance_fields, .. } =>
                            instance_fields.clone(),
                        TypeData::Variants(ref v) => v[variant_index.unwrap()].fields.clone(),
                        _ => return Ok(false),
                    };
                    let field_index = field_names.iter()
                        .enumerate()
                        .find(|(_, x)| x == &field_name).map(|(i, _)| i)
                        .ok_or(InterpreterErrorKind::MissingName(field_name.clone()).into())?;

                    // Get the value of that field and check if it's a match
                    let field_value = actual_field_values[field_index].clone();
                    if !match_against(&pattern, field_value, context)? {
                        return Ok(false)
                    }
                }
            }

            Ok(true)
        },

        PatternKind::PatternBinding(name, pattern) => {
            if match_against(&pattern, value.clone(), context)? {
                context.bindings.insert(name.clone(), value);
                Ok(true)
            } else {
                Ok(false)
            }
        },

        PatternKind::AnyBinding(name) => {
            context.bindings.insert(name.into(), value);
            Ok(true)
        },

        PatternKind::Discard => Ok(true),
    }
}

pub struct PatternMatchContext<'a> {
    pub interpreter: &'a mut Interpreter,
    pub bindings: HashMap<String, ValueRef>, 
    pub captured_self: ValueRef,
}
