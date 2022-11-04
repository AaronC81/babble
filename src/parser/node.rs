//! Implements the syntax tree built by the parser.

use crate::{source::Location, interpreter::{ValueRef, Interpreter, Variant, InterpreterError, Value}};

use super::{LexicalContextRef, Literal};

/// A node in the parse tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
    pub location: Location,
    pub context: LexicalContextRef,
}

/// A sequence of arguments (or lack thereof) describing which method is called, and with what
/// parameters.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendMessageComponents {
    /// The method call has no arguments.
    Unary(String),

    /// The method call has a sequence of named parameters and arguments.
    Parameterised(Vec<(String, SendMessageParameter)>),
}

impl SendMessageComponents {
    /// Converts this set of components to the method which they will call. For example, a call like
    /// `x set: 2 value: "Hello"` calls the method `set:hello:`.
    pub fn to_method_name(&self) -> String {
        match self {
            SendMessageComponents::Unary(s) => s.clone(),
            SendMessageComponents::Parameterised(params) => {
                params
                    .iter()
                    .map(|(p, _)| format!("{p}:"))
                    .collect::<Vec<_>>()
                    .concat()
            }
        }
    }

    /// Returns references to each child node.
    #[allow(unused)]
    pub fn child_nodes(&self) -> Vec<&Node> {
        match self {
            SendMessageComponents::Parameterised(params) => {
                params.iter().filter_map(|(_, param)| match param {
                    SendMessageParameter::Parsed(node) => Some(&**node),
                    _ => None,
                }).collect()
            },
            SendMessageComponents::Unary(_) => vec![],
        }
    }

    /// Returns mutable references to each child node.
    pub fn child_nodes_mut(&mut self) -> Vec<&mut Node> {
        match self {
            SendMessageComponents::Parameterised(params) => {
                params.iter_mut().filter_map(|(_, param)| match param {
                    SendMessageParameter::Parsed(node) => Some(&mut **node),
                    _ => None,
                }).collect()
            },
            SendMessageComponents::Unary(_) => vec![],
        }
    }

    /// Assuming that this set of components is used as part of a method definition, gets the
    /// internal names used within the definition body.
    /// 
    /// **Panics** if any parameters are not [`SendMessageParameter::Defined`].
    pub fn defined_internal_names(&self) -> Vec<String> {
        match self {
            SendMessageComponents::Unary(_) => vec![],
            SendMessageComponents::Parameterised(pl) => pl.iter().map(|(_, p)| match p {
                SendMessageParameter::Defined(id) => id.clone(),
                _ => unreachable!(),
            }).collect(),
        }
    }

    /// Evaluates the parameters in this set of components, or returns them if they are already
    /// evaluated.
    pub fn evaluate_parameters(&self, interpreter: &mut Interpreter) -> Result<Vec<ValueRef>, InterpreterError> {
        Ok(match self {
            SendMessageComponents::Unary(_) => vec![],
            SendMessageComponents::Parameterised(params) =>
                params.iter()
                    .map(|(_, p)| match p {
                        SendMessageParameter::Parsed(n) => interpreter.evaluate(n),
                        SendMessageParameter::Evaluated(v) => Ok(v.clone()),
                        SendMessageParameter::Defined(_) => unreachable!("defined parameters not valid in evaluation"),
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .iter()
                    .map(|v| Value::soft_copy(v.clone()))
                    .collect::<Vec<_>>()
        })
    }
}

/// A parameter passed to a message send.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendMessageParameter {
    /// The parameter has been parsed, but not yet evaluated.
    Parsed(Box<Node>),

    /// The parameter has already been evaluated.
    /// 
    /// Exists to allow intrinsic methods to call methods easily.
    Evaluated(ValueRef),

    /// The parameter is part of a method definition, not a call.
    Defined(String),
}

/// The kind of this node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Literal(Literal),

    SelfAccess,
    Identifier(String),
    SendMessage {
        receiver: Box<Node>,
        components: SendMessageComponents,
    },
    StatementSequence(Vec<Node>),
    Assignment {
        target: Box<Node>,
        value: Box<Node>,
    },
    Block {
        body: Box<Node>,
        parameters: Vec<String>,
        captures: Vec<String>,
    },
    EnumVariant {
        enum_type: Box<Node>,
        variant_name: String,
        components: SendMessageComponents,
    },
    ImplBlock {
        target: Box<Node>,
        body: Box<Node>,
    },
    FuncDefinition {
        parameters: SendMessageComponents,
        body: Box<Node>,
        is_static: bool,
        documentation: Option<String>,
    },
    EnumDefinition {
        name: String,
        variants: Vec<Variant>,
    },
    StructDefinition {
        name: String,
        instance_fields: Vec<String>,
        static_fields: Vec<String>,
    },
    MixinDefinition {
        name: String,
    },
    Use(Box<Node>),
}

pub trait NodeWalk {
    /// Calls `func` on this node and all child nodes.
    fn walk_children(&mut self, func: &mut impl FnMut(&mut Node));
}

impl NodeWalk for Node {
    fn walk_children(&mut self, func: &mut impl FnMut(&mut Node)) {
        match &mut self.kind {
            NodeKind::StatementSequence(seq) => {
                for node in seq {
                    func(node);
                }
            },
            NodeKind::SendMessage { receiver, components } => {
                func(receiver);
                for node in components.child_nodes_mut() {
                    func(node);
                }
            },
            NodeKind::Assignment { target, value } => {
                func(target);
                func(value);
            },
            NodeKind::Block { body, parameters: _, captures: _ } => {
                func(body);
            },
            NodeKind::EnumVariant { enum_type, variant_name: _, components } => {
                func(enum_type);
                for node in components.child_nodes_mut() {
                    func(node);
                }
            },
            NodeKind::ImplBlock { target, body } => {
                func(target);
                func(body);
            },
            NodeKind::FuncDefinition { parameters, body, is_static: _, documentation: _ } => {
                for node in parameters.child_nodes_mut() {
                    func(node);
                }
                func(body);
            },
            NodeKind::Use(mixin) => {
                func(mixin);
            }
            NodeKind::Literal(l) => l.walk_children(func),

            | NodeKind::SelfAccess
            | NodeKind::EnumDefinition { name: _, variants: _ }
            | NodeKind::StructDefinition { name: _, instance_fields: _, static_fields: _ }
            | NodeKind::MixinDefinition { name: _ }
            | NodeKind::Identifier(_) => (),
        }
    }
}

impl NodeWalk for Literal {
    fn walk_children(&mut self, func: &mut impl FnMut(&mut Node)) {
        match self {
            Literal::Array(items) => {
                for node in items {
                    node.walk_children(func);
                }
            }
            _ => (),
        }
    }
}
