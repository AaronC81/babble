use crate::{source::Location, interpreter::{ValueRef, Interpreter, Variant, InterpreterError, Value}};

use super::LexicalContextRef;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
    pub location: Location,
    pub context: LexicalContextRef,
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

    pub fn defined_internal_names(&self) -> Vec<String> {
        match self {
            SendMessageComponents::Unary(_) => vec![],
            SendMessageComponents::Parameterised(pl) => pl.iter().map(|(_, p)| match p {
                SendMessageParameter::Defined(id) => id.clone(),
                _ => unreachable!(),
            }).collect(),
        }
    }

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendMessageParameter {
    Parsed(Box<Node>),
    Evaluated(ValueRef),
    Defined(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    IntegerLiteral(u64),
    StringLiteral(String),
    TrueLiteral,
    FalseLiteral,
    NullLiteral,
    SelfLiteral,

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
    },
    EnumDefinition {
        name: String,
        variants: Vec<Variant>,
    },
    StructDefinition {
        name: String,
        fields: Vec<String>,
    },
    MixinDefinition {
        name: String,
    },
    Use(Box<Node>),
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
            NodeKind::FuncDefinition { parameters, body, is_static: _ } => {
                for node in parameters.child_nodes_mut() {
                    func(node);
                }
                func(body);
            },
            NodeKind::Use(mixin) => {
                func(mixin);
            }

            NodeKind::IntegerLiteral(_)
            | NodeKind::StringLiteral(_) 
            | NodeKind::TrueLiteral
            | NodeKind::FalseLiteral
            | NodeKind::NullLiteral 
            | NodeKind::SelfLiteral
            | NodeKind::EnumDefinition { name: _, variants: _ }
            | NodeKind::StructDefinition { name: _, fields: _ }
            | NodeKind::MixinDefinition { name: _ }
            | NodeKind::Identifier(_) => (),
        }
    }
}
