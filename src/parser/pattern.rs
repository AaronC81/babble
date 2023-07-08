//! Implements patterns which values can be matched against, optionally extracting certain fields
//! from the structure of values.
//! 
//! Patterns do not currently use any special syntax - they are parsed as if they were regular 
//! nodes, and then converted into a pattern as a second step.

use std::collections::HashMap;
use std::fmt::Display;

use crate::{parser::{SendMessageComponents, SendMessageParameter, Node, NodeKind}, source::Location};

use super::{Literal, SugarNodeKind, LexicalContextRef, BlockParameters, NodeFactory};

/// An error occurred when parsing a node tree into a pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternParseError {
    /// The given node is not allowed to appear at this location in a pattern.
    InvalidNode(Node),
}

impl Display for PatternParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternParseError::InvalidNode(_) => write!(f, "invalid node in pattern"),
        }
    }
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
            | NodeKind::Use { .. }
                => Err(PatternParseError::InvalidNode(node)),
        }
    }

    /// Converts this pattern into a sequence of nodes. The nodes should check whether the pattern
    /// is a match against the local variable described by `candidate`. If there is *not* a match,
    /// execute the node described by `no_match`.
    /// 
    /// Nodes created for the desugar should use the `location` and `context` provided.
    /// 
    /// If intermediate local variables need to be created, a unique `prefix` is provided which can
    /// be used to disambiguate them from any other variables in the scope. Intermediate locals
    /// should generally be used where possible if calling a method on the candidate, rather than
    /// assuming that methods are idempotent.
    pub fn desugar(&self, candidate: &Node, prefix: &str, no_match: &NodeKind, location: &Location, context: &LexicalContextRef) -> Vec<Node> {
        let factory = NodeFactory::new(location, context);

        match &self.kind {
            // For literals, call `equals:` on the *expected* value, in case the provided one 
            // doesn't implement `Equatable` (or does so in a broken way)
            PatternKind::Literal(l) => vec![
                // <check> $ ifFalse: [ <no_match> ]
                factory.build(NodeKind::SendMessage {
                    // <expected> equals: <actual>
                    receiver: factory.build_boxed(NodeKind::SendMessage {
                        receiver: factory.build_boxed(NodeKind::Literal(l.clone())),
                        components: SendMessageComponents::Parameterised(vec![(
                            "equals".to_string(),
                            SendMessageParameter::CallArgument(Box::new(candidate.clone())),
                        )])
                    }),
                    components: SendMessageComponents::Parameterised(vec![(
                        "ifFalse".to_string(),
                        SendMessageParameter::CallArgument(
                            factory.build_boxed(NodeKind::Block {
                                body: factory.build_boxed(no_match.clone()),
                                parameters: BlockParameters::Named(vec![]),
                                captures: vec![],
                            }),
                        ),
                    )])
                }),
            ],

            PatternKind::Array(_) => todo!(),

            // TODO: behaviour on missing field... error, or match fail?
            PatternKind::Fields { type_name, variant_name, fields } => todo!(),

            // This doesn't do any checking - just bind the candidate to the binding
            PatternKind::AnyBinding(binding_name) => vec![
                factory.build(NodeKind::Assignment {
                    target: factory.build_boxed(NodeKind::Identifier(binding_name.clone())),
                    value: Box::new(candidate.clone()),
                }),
            ],

            PatternKind::PatternBinding(_, _) => todo!(),

            // Nothing to do!
            PatternKind::Discard => vec![],
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
