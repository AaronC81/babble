//! Defines an instruction set for representing compiled Babble code, and a compiler which can 
//! transform a parsed node tree into those instructions.
//! 
//! The instruction set is stack-based, with instructions designed so that every valid expression
//! will leave exactly one value on the stack. This design choice means statement sequences can be
//! compiled with `pop`s interspersed, e.g: `<stmt 1>, pop, <stmt 2>, pop, <stmt 3>`, leaving the
//! implicit return value of the block as the only item left on the stack.

// TODO: how do matchblocks work with this?

use std::fmt::Display;

use crate::parser::{Node, Literal, NodeKind, SendMessageComponents, SendMessageParameter, BlockParameters};

use super::{Value, InterpreterError, TypeData, InterpreterErrorKind, MethodLocality};

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Resolves a value by name and pushes it onto the stack.
    Get(String),

    /// Pushes `self` onto the stack.
    GetSelf,

    /// Peeks the stack, and sets a local variable resolved by name to that value.
    Set(String),

    /// Pops the stack to get a target, and then peeks the stack to get a value. Sets the field with
    /// the given name on the target to the value.
    SetField(String),

    /// Peeks the stack, and sets `self` to that value.
    SetSelf,

    /// Pops the stack, discarding the value.
    Pop,
    
    /// Pushes a fixed literal value onto the stack.
    Push(Literal),

    /// Pushes a block onto the stack. The block takes the named parameters, captures the named
    /// local variables, and has a body of instructions.
    PushBlock {
        parameters: BlockParameters,
        captures: Vec<String>,
        body: Vec<Instruction>,
    },

    /// Calls a method on a receiver. Pops arguments, last-first, then the receiver. Pushes the
    /// return value.
    /// 
    /// The number of arguments is determined by the method name.
    Call(String),

    /// Constructs an enum variant value. Pops arguments, last-first, then the enum value. Pushes
    /// the constructed enum variant.
    /// 
    /// The number of arguments is determined by the argument labels given.
    NewVariant {
        name: String,
        labels: Vec<String>,
    },

    /// Pops the stack to get a block, then peeks it to get a target. Executes the block as an
    /// `impl` context on the target.
    Impl,

    /// Defines a `struct`, `enum`, or `mixin`, based on the given `TypeData`. Pushes the type onto
    /// the stack.
    DefType(String, TypeData),

    /// Peeks the stack to get a mixin, which is imported into the current type. Must be inside an
    /// `impl` context.
    Use,

    /// Peeks the stack to get a block, and defines a function with that block as its body.
    DefFunc {
        name: String,
        locality: MethodLocality,
        documentation: Option<String>,
    },
}

impl Instruction {
    pub fn compile(node: Node) -> Result<Vec<Instruction>, InterpreterError> {
        Ok(match node.kind {
            NodeKind::Literal(l) => vec![Instruction::Push(l)],
            NodeKind::SelfAccess => vec![Instruction::GetSelf],
            NodeKind::Identifier(i) => vec![Instruction::Get(i)],

            NodeKind::StatementSequence(stmts) => {
                // Compile with interspersed `pop`s
                let mut instructions = vec![];
                let mut is_first_stmt = true;
                for stmt in stmts {
                    if !is_first_stmt {
                        instructions.push(Instruction::Pop);
                    } else {
                        is_first_stmt = false;
                    }
                    instructions.extend(Instruction::compile(stmt)?);
                }
                instructions
            },

            NodeKind::Block { body, parameters, captures } =>
                vec![Instruction::PushBlock {
                    parameters,
                    captures,
                    body: Instruction::compile(*body)?,
                }],

            NodeKind::EnumVariant { enum_type, variant_name, components } => {
                let mut instructions = Instruction::compile(*enum_type)?;

                let mut labels = vec![];
                match components {
                    SendMessageComponents::Blank => (), // No labels, nothing to do
                    SendMessageComponents::Parameterised(params) => {
                        for (name, param) in params {
                            labels.push(name);
                            match param {
                                SendMessageParameter::Parsed(n) => instructions.extend(Instruction::compile(*n)?),

                                SendMessageParameter::Evaluated(_)
                                | SendMessageParameter::Defined(_) => unreachable!(),
                            }
                        }
                    },
                    SendMessageComponents::Unary(_) => unreachable!("enum constructor cannot be unary"),
                }
                instructions.push(Instruction::NewVariant { name: variant_name, labels });
                instructions
            },

            NodeKind::Assignment { target, value } => {
                // What are we assigning to?
                match target.kind {
                    // Local variable
                    NodeKind::Identifier(i) => {
                        let mut instructions = Instruction::compile(*value)?;
                        instructions.push(Instruction::Set(i));
                        instructions
                    },

                    // Self
                    NodeKind::SelfAccess => {
                        let mut instructions = Instruction::compile(*value)?;
                        instructions.push(Instruction::SetSelf);
                        instructions
                    }

                    // Field
                    NodeKind::SendMessage { receiver, components: SendMessageComponents::Unary(field_name) } => {
                        let mut instructions = Instruction::compile(*receiver)?;
                        instructions.extend(Instruction::compile(*value)?);
                        instructions.push(Instruction::SetField(field_name));
                        instructions
                    }

                    _ => return Err(InterpreterErrorKind::InvalidAssignmentTarget.into()),
                }
            }

            NodeKind::SendMessage { receiver, components } => {
                let mut instructions = Instruction::compile(*receiver)?;

                let name = components.to_method_name();
                match components {
                    SendMessageComponents::Parameterised(args) => {
                        for (_, arg) in args {
                            match arg {
                                SendMessageParameter::Parsed(n) => instructions.extend(Instruction::compile(*n)?),

                                SendMessageParameter::Evaluated(_)
                                | SendMessageParameter::Defined(_) => unreachable!(),
                            }
                        }
                    },
                    
                    SendMessageComponents::Unary(_) => (),
                    SendMessageComponents::Blank => unreachable!("method call cannot be blank"),
                }

                instructions.push(Instruction::Call(name));
                instructions
            }

            NodeKind::ImplBlock { target, body } => {
                let inner = Instruction::compile(*body)?;

                let mut instructions = Instruction::compile(*target)?;
                instructions.push(Instruction::PushBlock {
                    parameters: BlockParameters::Named(vec![]),
                    captures: vec![],
                    body: inner,
                });
                instructions.push(Instruction::Impl);
                instructions
            },

            NodeKind::FuncDefinition { parameters, body, is_static, documentation } => {
                let inner = Instruction::compile(*body)?;
                let name = parameters.to_method_name();

                vec![
                    Instruction::PushBlock {
                        parameters: match parameters {
                            SendMessageComponents::Parameterised(p) => BlockParameters::Named(p.into_iter().map(|(n, _)| n).collect()),
                            SendMessageComponents::Unary(_) => BlockParameters::Named(vec![]),
                            SendMessageComponents::Blank => unreachable!("cannot define function with blank parameters"),
                        },
                        captures: vec![],
                        body: inner,
                    },
                    Instruction::DefFunc {
                        name,
                        locality: if is_static { MethodLocality::Static } else { MethodLocality::Instance },
                        documentation,
                    },
                ]
            }
            
            NodeKind::EnumDefinition { name, variants } => 
                vec![Instruction::DefType(name, TypeData::Variants(variants))],
            NodeKind::StructDefinition { name, instance_fields, static_fields } =>
                vec![Instruction::DefType(name, TypeData::Fields { instance_fields, static_fields })],
            NodeKind::MixinDefinition { name } =>
                vec![Instruction::DefType(name, TypeData::Mixin)],
            NodeKind::Use(target) => {
                let mut instructions = Self::compile(*target)?;
                instructions.push(Instruction::Use);
                instructions
            },

            NodeKind::Sugar(_) => unreachable!("sugar nodes should be expanded before compilation"),
        })
    }
}

fn indent(s: String) -> String {
    s.split("\n").map(|l| format!("  {}", l)).collect::<Vec<_>>().join("\n")
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Get(n) => write!(f, "get {}", n),
            Instruction::GetSelf => write!(f, "get (self)"),
            Instruction::Set(n) => write!(f, "set {}", n),
            Instruction::SetField(n) => write!(f, "set field {}", n),
            Instruction::SetSelf => write!(f, "set (self)"),
            Instruction::Pop => write!(f, "pop"),
            Instruction::Push(l) => write!(f, "push {}", l),
            Instruction::PushBlock { parameters, captures: _, body } =>
                write!(f, "push block [ | {} |\n{}\n]",
                    match parameters {
                        BlockParameters::Named(names) => names.join(" "),
                        BlockParameters::Patterned { .. } => todo!("patterns not supported"),
                    },
                    indent(body.iter().map(|i| format!("{}", i)).collect::<Vec<_>>().join("\n")),
                ),
            Instruction::Call(n) => write!(f, "call {}", n),
            Instruction::NewVariant { name, labels } =>
                write!(f, "new variant {} {}",
                    name,
                    if labels.is_empty() { "".into() } else { format!("{}:", labels.join(":")) }
                ),
            Instruction::Impl => write!(f, "impl"),
            Instruction::DefType(name, _) =>
                write!(f, "def type {} ...", name), // TODO: type data
            Instruction::Use => write!(f, "use"),
            Instruction::DefFunc { name, locality, documentation: _ } =>
                write!(f, "def func {} {} ...", name, match locality {
                    MethodLocality::Instance => "(instance)",
                    MethodLocality::Static => "(static)",
                }), // TODO: documentation
        }
    }
}
