//! Implements patterns which values can be matched against, optionally extracting certain fields
//! from the structure of values.
//! 
//! Patterns do not currently use any special syntax - they are parsed as if they were regular 
//! nodes, and then converted into a pattern as a second step.

use std::collections::HashMap;

use crate::parser::{SendMessageComponents, SendMessageParameter, Node, NodeKind};

use crate::interpreter::{ValueRef, InterpreterError, Interpreter, Value, InterpreterErrorKind, TypeInstance, TypeData};

use super::{Literal, SugarNodeKind};

/// An error occurred when parsing a node tree into a pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternParseError {
    /// The given node is not allowed to appear at this location in a pattern.
    InvalidNode(Node),

    /// The pattern being matched against is a [`Literal`] which is impure (i.e. constructing it
    /// could have side effects). This should be treated as an internal error, as generally the
    /// parsing step should disallow this.
    ImpureLiteral,
}

/// A pattern, describing a data layout that a value can be matched against, optionally extracting
/// values from the value.
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

    pub fn new_array(patterns: Vec<Pattern>) -> Self {
        Self::new(PatternKind::Array(patterns))
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

    pub fn new_fields(type_name: Option<String>, variant_name: Option<String>, fields: Vec<(String, Pattern)>) -> Self {
        Self::new(PatternKind::Fields {
            type_name,
            variant_name,
            fields,
        })
    }

    /// Matches the given value against this pattern, returning a boolean indicating whether the 
    /// pattern matched.
    /// 
    /// If the pattern contains bindings, the bound values are recorded in the provided `context`.
    pub fn match_against(&self, value: ValueRef, context: &mut PatternMatchContext) -> Result<bool, InterpreterError> {
        match &self.kind {
            PatternKind::Literal(expected) => {
                let expected_value = expected.instantiate(context.interpreter)?;
                Ok(
                    context.interpreter.send_message(value, "equals:", vec![expected_value])?
                        .borrow().to_boolean().unwrap()
                )
            },

            PatternKind::Array(patterns) => {
                // Extract array
                let mut value = value.borrow_mut();
                let array = value.to_array();
                let Ok(array) = array else { return Ok(false) };

                // Trying matching against each pattern
                if patterns.len() != array.len() {
                    return Ok(false);
                }
                for (pattern, value) in patterns.iter().zip(array.iter()) {
                    if !pattern.match_against(value.clone(), context)? {
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
                        vec![context.captured_self.clone()],
                    )?;
                    let type_value = type_value.borrow().to_type()?;
                    type_value
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

    /// Extracts the list of all bindings which could be bound when matching a pattern.
    pub fn all_bindings(&self) -> Vec<String> {
        match &self.kind {
            PatternKind::Literal(_) => vec![],
            PatternKind::Array(array) => array.iter().flat_map(|p| p.all_bindings()).collect(),
            PatternKind::Fields { fields, .. } =>
                fields.iter().flat_map(|(_, pattern)| pattern.all_bindings()).collect(),
            PatternKind::PatternBinding(name, _) => vec![name.clone()],
            PatternKind::AnyBinding(name) => vec![name.clone()],
            PatternKind::Discard => vec![],
        }
    }

    /// Parse a node tree into a pattern.
    pub fn parse(node: Node) -> Result<Self, PatternParseError> {
        match node.kind {
            // Literals
            NodeKind::Literal(l) => Ok(Pattern::new_literal(l)),

            // Arrays
            NodeKind::Array(items) =>
                Ok(Pattern::new_array(
                    items.into_iter()
                        .map(Pattern::parse)
                        .collect::<Result<_, _>>()?
                )),
            
            // Compounds
            NodeKind::SendMessage { receiver, components } => {
                let NodeKind::Identifier(type_name) = receiver.kind else {
                    return Err(PatternParseError::InvalidNode(*receiver))
                };

                let fields = match components {
                    SendMessageComponents::Blank => vec![],
                    SendMessageComponents::Unary(_) => unreachable!(),
                    SendMessageComponents::Parameterised(params) =>
                        params.iter().map(|(name, param)| match param {
                            SendMessageParameter::CallArgument(node) =>
                                Self::parse(*node.clone()).map(|p| (name.into(), p)),
                            _ => unreachable!(),
                        }).collect::<Result<Vec<_>, _>>()?,
                };

                Ok(Pattern::new_fields(Some(type_name), None, fields))
            },
            NodeKind::EnumVariant { enum_type, variant_name, components } => {
                // Extract type name, or use `None` if we've used shorthand variant syntax
                // (Conveniently, variants get parsed before sugar expansion, so we can just check
                // for that!)
                let type_name = if let NodeKind::Identifier(type_name) = enum_type.kind {
                    Some(type_name)
                } else if let NodeKind::Sugar(SugarNodeKind::ShorthandVariantConstructor) = enum_type.kind {
                    None
                } else {
                    return Err(PatternParseError::InvalidNode(*enum_type))
                };

                let fields = match components {
                    SendMessageComponents::Blank => vec![],
                    SendMessageComponents::Unary(_) => unreachable!(),
                    SendMessageComponents::Parameterised(params) =>
                        params.iter().map(|(name, param)| match param {
                            SendMessageParameter::CallArgument(node) =>
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
                } else if i.chars().next().unwrap().is_uppercase() {
                    Ok(Pattern::new_fields(Some(i), None, vec![]))
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
            | NodeKind::Sugar(_)
            | NodeKind::Use(_)
                => Err(PatternParseError::InvalidNode(node)),
        }
    }
}

/// Describes the requirements of a pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternKind {
    /// The value must be equal to the provided literal, according to `equals:`.
    Literal(Literal),

    /// The value must be an `Array` with items matching the provided patterns.
    Array(Vec<Pattern>),

    /// The value must be an instance of the given struct or enum type, and optionally have a set of
    /// fields which must match their corresponding patterns.
    Fields {
        type_name: Option<String>,
        variant_name: Option<String>,
        fields: Vec<(String, Pattern)>,
    },

    /// Matches any value and binds it to the given name.
    AnyBinding(String),

    /// Matches any value matching the inner pattern, and if it matches, binds it to the given name.
    PatternBinding(String, Box<Pattern>),

    /// Matches any value.
    Discard,
}

pub struct PatternMatchContext<'a> {
    pub interpreter: &'a mut Interpreter,
    pub bindings: HashMap<String, ValueRef>, 
    pub captured_self: ValueRef,
}
