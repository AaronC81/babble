use std::collections::HashMap;

use crate::parser::{SendMessageComponents, SendMessageParameter, Node, NodeKind};

use crate::interpreter::{ValueRef, InterpreterError, Interpreter, Value, InterpreterErrorKind, TypeInstance, TypeData};

use super::Literal;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternParseError {
    InvalidNode(Node),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern {
    pub kind: PatternKind,
}

impl Pattern {
    pub fn new(kind: PatternKind) -> Self {
        Self { kind }
    }

    pub fn new_literal(value: Literal) -> Self {
        Self::new(PatternKind::Literal(value))
    }

    pub fn new_any_binding(name: &str) -> Self {
        Self::new(PatternKind::AnyBinding(name.into()))
    }

    pub fn new_pattern_binding(name: &str, pattern: Pattern) -> Self {
        Self::new(PatternKind::PatternBinding(name.into(), Box::new(pattern)))
    }

    pub fn new_discard() -> Self {
        Self::new(PatternKind::Discard)
    }

    pub fn new_fields(type_name: String, variant_name: Option<String>, fields: Vec<(String, Pattern)>) -> Self {
        Self::new(PatternKind::Fields {
            type_name,
            variant_name,
            fields,
        })
    }

    pub fn match_against(&self, value: ValueRef, context: &mut PatternMatchContext) -> Result<bool, InterpreterError> {
        match &self.kind {
            PatternKind::Literal(expected) => {
                // TODO: currently probably produces unexpected results for impure literals
                let expected_value = expected.instantiate(context.interpreter)?;
                Ok(
                    context.interpreter.send_message(
                        value,
                        &SendMessageComponents::Parameterised(vec![
                            ("equals".to_string(), SendMessageParameter::Evaluated(expected_value)),
                        ])
                    )?.borrow().to_boolean().unwrap()
                )
            },
            PatternKind::Fields { type_name, variant_name, fields } => {
                // Resolve the type being matched against
                let Some(type_value) = context.interpreter.resolve_type(type_name) else {
                    return Err(InterpreterErrorKind::MissingName(type_name.into()).into())
                };

                // If there's also a variant involved, resolve that too
                let type_value_borrow = type_value.borrow();
                let variant_value = variant_name
                    .as_ref()
                    .map(|v| type_value_borrow.resolve_variant(&v))
                    .transpose()?;
                let variant_index = variant_value.map(|(i, _)| i);
                drop(type_value_borrow);

                // Check pattern type against actual value's type
                if value.borrow().type_instance.get_type(&context.interpreter) != type_value {
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
                        if !pattern.match_against(field_value, context)? {
                            return Ok(false)
                        }
                    }
                }

                Ok(true)
            },
            PatternKind::PatternBinding(name, pattern) => {
                if pattern.match_against(value.clone(), context)? {
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

    pub fn all_bindings(&self) -> Vec<String> {
        match &self.kind {
            PatternKind::Literal(_) => vec![],
            PatternKind::Fields { fields, .. } =>
                fields.iter().flat_map(|(_, pattern)| pattern.all_bindings()).collect(),
            PatternKind::PatternBinding(name, _) => vec![name.clone()],
            PatternKind::AnyBinding(name) => vec![name.clone()],
            PatternKind::Discard => vec![],
        }
    }

    pub fn parse(node: Node) -> Result<Self, PatternParseError> {
        match node.kind {
            // Literals
            NodeKind::Literal(l) => Ok(Pattern::new_literal(l)),

            // Compounds - all TODO currently
            NodeKind::SendMessage { receiver, components } => todo!(),
            NodeKind::EnumVariant { enum_type, variant_name, components } => {
                let NodeKind::Identifier(type_name) = enum_type.kind else {
                    return Err(PatternParseError::InvalidNode(*enum_type))
                };

                let fields = match components {
                    SendMessageComponents::Blank => vec![],
                    SendMessageComponents::Unary(_) => unreachable!(),
                    SendMessageComponents::Parameterised(params) =>
                        params.iter().map(|(name, param)| match param {
                            SendMessageParameter::Parsed(node) =>
                                Self::parse(*node.clone()).map(|p| (name.into(), p)),
                            _ => unreachable!(),
                        }).collect::<Result<Vec<_>, _>>()?,
                };

                Ok(Pattern::new_fields(type_name, Some(variant_name), fields))
            },

            // Bindings
            NodeKind::Identifier(i) =>
                if i == "_" {
                    Ok(Pattern::new_discard())
                } else {
                    Ok(Pattern::new_any_binding(&i))
                }
            NodeKind::Assignment { target, value } =>
                if let NodeKind::Identifier(i) = target.kind {
                    Self::parse(*value).map(|p| Pattern::new_pattern_binding(&i, p))
                } else {
                    Err(PatternParseError::InvalidNode(*target))
                },
            
            // Invalid
            NodeKind::StatementSequence(_)
            | NodeKind::Block { .. }
            | NodeKind::SelfAccess
            | NodeKind::ImplBlock { .. }
            | NodeKind::FuncDefinition { .. }
            | NodeKind::EnumDefinition { .. }
            | NodeKind::StructDefinition { .. }
            | NodeKind::MixinDefinition { .. }
            | NodeKind::Use(_)
                => Err(PatternParseError::InvalidNode(node)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternKind {
    Literal(Literal),
    Fields {
        type_name: String,
        variant_name: Option<String>,
        fields: Vec<(String, Pattern)>,
    },
    AnyBinding(String),
    PatternBinding(String, Box<Pattern>),
    Discard,
}

pub struct PatternMatchContext<'a> {
    pub interpreter: &'a mut Interpreter,
    pub bindings: HashMap<String, ValueRef>, 
}
