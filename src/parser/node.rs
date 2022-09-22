use crate::{source::Location, interpreter::ValueRef};

use super::LexicalContextRef;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
    pub location: Location,
    pub context: LexicalContextRef,
}

impl Node {
    pub fn new(kind: NodeKind, location: Location, context: LexicalContextRef) -> Self {
        Node { kind, location, context }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendMessageComponents {
    Unary(String),
    Parameterised(Vec<(String, SendMessageParameter)>),
}

impl SendMessageComponents {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendMessageParameter {
    Parsed(Box<Node>),
    Evaluated(ValueRef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    IntegerLiteral(u64),
    StringLiteral(String),
    TrueLiteral,
    FalseLiteral,
    NullLiteral,

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
}

pub trait NodeWalk {
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
            NodeKind::Block { body, parameters, captures } => {
                func(body);
            },
            NodeKind::EnumVariant { enum_type, variant_name, components } => {
                func(enum_type);
                for node in components.child_nodes_mut() {
                    func(node);
                }
            },

            NodeKind::IntegerLiteral(_)
            | NodeKind::StringLiteral(_) 
            | NodeKind::TrueLiteral
            | NodeKind::FalseLiteral
            | NodeKind::NullLiteral 
            | NodeKind::Identifier(_) => (),
        }
    }
}