//! Implements the syntax tree built by the parser.

use crate::{source::Location, interpreter::{ValueRef, Interpreter, Variant, InterpreterError, Value, MethodVisibility}, tokenizer::TokenKind};

use super::{LexicalContextRef, Literal, Pattern};

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
    /// The call has no associated method information, not even a method name.
    /// 
    /// This is currently only used for one special case - enum variant instantiation where the enum
    /// variant has no fields.
    Blank,

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
            SendMessageComponents::Blank => unreachable!(),
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
                    SendMessageParameter::CallArgument(node) => Some(&**node),
                    _ => None,
                }).collect()
            },
            SendMessageComponents::Unary(_) | SendMessageComponents::Blank => vec![],
        }
    }

    /// Returns mutable references to each child node.
    pub fn child_nodes_mut(&mut self) -> Vec<&mut Node> {
        match self {
            SendMessageComponents::Parameterised(params) => {
                params.iter_mut().filter_map(|(_, param)| match param {
                    SendMessageParameter::CallArgument(node) => Some(&mut **node),
                    _ => None,
                }).collect()
            },
            SendMessageComponents::Unary(_) | SendMessageComponents::Blank => vec![],
        }
    }

    /// Assuming that this set of components is used as part of a method definition, gets the
    /// internal names used within the definition body.
    /// 
    /// **Panics** if any parameters are not [`SendMessageParameter::Defined`].
    pub fn defined_internal_names(&self) -> Vec<String> {
        match self {
            SendMessageComponents::Unary(_) | SendMessageComponents::Blank => vec![],
            SendMessageComponents::Parameterised(pl) => pl.iter().map(|(_, p)| match p {
                SendMessageParameter::DefinitionParameter(id) => id.clone(),
                _ => unreachable!(),
            }).collect(),
        }
    }
}

/// A parameter passed to a message send.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendMessageParameter {
    /// The parameter is a node tree passed to a method call.
    CallArgument(Box<Node>),

    /// The parameter is part of a method definition, not a call.
    DefinitionParameter(String),
}

/// The parameters taken by a block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockParameters {
    /// The block takes simple, named parameters.
    Named(Vec<String>),

    /// The block takes any number of simple parameters, and collects them into a named array.
    All(String),

    /// The block takes patterns as parameters, and depending on `fatal`, will either wrap the
    /// block's return value in a `Match` or cause a fatal error to indicate that the arguments did
    /// not match a pattern.
    Patterned {
        patterns: Vec<Pattern>,
        fatal: bool,
    }
}

/// The kind of this node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Literal(Literal),
    Array(Vec<Node>),

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
        parameters: BlockParameters,
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
        visibility: MethodVisibility,
    },
    EnumDefinition {
        name: String,
        variants: Vec<Variant>,
        documentation: Option<String>,
    },
    StructDefinition {
        name: String,
        instance_fields: Vec<String>,
        static_fields: Vec<String>,
        documentation: Option<String>,
    },
    MixinDefinition {
        name: String,
        documentation: Option<String>,
    },
    Use {
        mixin: Box<Node>,
        is_static: bool,
    },

    Sugar(SugarNodeKind),
}

/// A sub-kind of a [NodeKind::Sugar].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SugarNodeKind {
    /// The node is a `return` statement, which will be desugared into a throw/catch.
    Return(Box<Node>),

    /// The node is a shorthand block, which will be desugared into a normal block which takes one
    /// argument and calls the given method name on it.
    ShorthandBlock(String),

    /// The node is a binary operation, which will be desugared into a parameterised method call.
    BinaryMessage {
        left: Box<Node>,
        right: Box<Node>,
        op: BinaryOperation,
    },

    /// The node is the type for a shorthand enum variant.
    ShorthandVariantConstructor,

    /// The node is an interpolated string, which will be desugared into a sequence of `concat:`
    /// and `repr` calls.
    StringInterpolation(Vec<Node>),
}

/// The binary operation used by a [SugarNodeKind::BinaryMessage].
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl BinaryOperation {
    /// Converts a [TokenKind] into the corresponding [BinaryOperation] which it represents.
    pub fn from_token_kind(kind: &TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(Self::Add),
            TokenKind::Dash => Some(Self::Subtract),
            TokenKind::Star => Some(Self::Multiply),
            TokenKind::ForwardSlash => Some(Self::Divide),
            _ => None,
        }
    }

    /// Returns the one-argument **parameter** name which corresponds to this operation. This does
    /// not include the ending colon, even though the method takes one parameter, for simplicity of
    /// building a node tree.
    pub fn parameter_name(&self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Subtract => "sub",
            Self::Multiply => "mul",
            Self::Divide => "div",
        }
    }
}
