//! Defines an instruction set for representing compiled Babble code, and a compiler which can 
//! transform a parsed node tree into those instructions.
//! 
//! The instruction set is stack-based, with instructions designed so that every valid expression
//! will leave exactly one value on the stack. This design choice means statement sequences can be
//! compiled with `pop`s interspersed, e.g: `<stmt 1>, pop, <stmt 2>, pop, <stmt 3>`, leaving the
//! implicit return value of the block as the only item left on the stack.

use std::{fmt::Display, slice::Iter, rc::Rc};

use crate::{parser::{Node, Literal, NodeKind, SendMessageComponents, SendMessageParameter, BlockParameters}, source::{Location, SourceFile}};

use super::{Value, InterpreterError, TypeData, InterpreterErrorKind, MethodLocality, MethodVisibility};

#[derive(Debug)]
pub struct InstructionBlock(Vec<Instruction>);

impl InstructionBlock {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn extend(&mut self, other: impl IntoIterator<Item = Instruction>) {
        self.0.extend(other.into_iter());
    }

    pub fn push(&mut self, instruction: Instruction) {
        self.0.push(instruction);
    }

    pub fn iter(&self) -> Iter<Instruction> {
        self.0.iter()
    }

    pub fn as_vec(&self) -> &Vec<Instruction> {
        &self.0
    }

    pub fn source_file(&self) -> Rc<SourceFile> {
        self.0.first().unwrap().location.source_file.clone()
    }

    pub fn rc(self) -> InstructionBlockRef {
        Rc::new(self)
    }
}

impl Default for InstructionBlock {
    fn default() -> Self {
        Self::new()
    }
}

pub type InstructionBlockRef = Rc<InstructionBlock>;

impl IntoIterator for InstructionBlock {
    type Item = Instruction;
    type IntoIter = std::vec::IntoIter<Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a InstructionBlock {
    type Item = &'a Instruction;
    type IntoIter = std::slice::Iter<'a, Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl From<Vec<Instruction>> for InstructionBlock {
    fn from(v: Vec<Instruction>) -> Self {
        InstructionBlock(v)
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub location: Location,
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
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
        body: InstructionBlockRef,
    },

    /// Calls a method on a receiver. Pops arguments, last-first, then the receiver. Pushes the
    /// return value.
    /// 
    /// The number of arguments is calculated from the method name.
    Call {
        name: String,
        arity: usize,
    },

    /// Peeks the item on the top of the stack, and pushes another copy of it.
    Duplicate,

    /// Constructs an enum variant value. Pops the enum type, then arguments, last-first. Pushes
    /// the constructed enum variant.
    /// 
    /// The number of arguments is determined by the argument labels given.
    /// 
    /// Note that this pop order is different to function calls, since we should validate the
    /// variant fields before constructing the value, otherwise we could imbalance the stack.
    NewVariant {
        name: String,
        labels: Vec<String>,
    },

    /// Pops the stack to get a block, then peeks it to get a target. Executes the block as an
    /// `impl` context on the target.
    Impl,

    /// Defines a `struct`, `enum`, or `mixin`, based on the given `TypeData`. Pushes the type onto
    /// the stack.
    DefType {
        name: String,
        data: TypeData,
        documentation: Option<String>,
    },

    /// Peeks the stack to get a mixin, which is imported into the current type. Must be inside an
    /// `impl` context.
    Use,

    /// Peeks the stack to get a block, and defines a function with that block as its body.
    DefFunc {
        name: String,
        locality: MethodLocality,
        documentation: Option<String>,
        visibility: MethodVisibility,
    },
}

impl InstructionKind {
    pub fn with_loc(self, location: &Location) -> Instruction {
        Instruction { kind: self, location: location.clone() }
    }
}

pub fn compile(node: Node) -> Result<InstructionBlock, InterpreterError> {
    let loc = node.location;

    Ok(
        match node.kind {
            NodeKind::Literal(l) => vec![InstructionKind::Push(l).with_loc(&loc)].into(),
            NodeKind::SelfAccess => vec![InstructionKind::GetSelf.with_loc(&loc)].into(),
            NodeKind::Identifier(i) => vec![InstructionKind::Get(i).with_loc(&loc)].into(),

            NodeKind::Array(items) => {
                let mut instructions: InstructionBlock = vec![
                    InstructionKind::Get("Array".into()).with_loc(&loc),
                    InstructionKind::Call {
                        name: "new".into(),
                        arity: 0,
                    }.with_loc(&loc),
                ].into();

                for item in items {
                    instructions.push(InstructionKind::Duplicate.with_loc(&loc));
                    instructions.extend(compile(item)?);
                    instructions.push(InstructionKind::Call {
                        name: "append:".into(),
                        arity: 1,
                    }.with_loc(&loc));
                    instructions.push(InstructionKind::Pop.with_loc(&loc));
                }

                instructions
            }

            NodeKind::StatementSequence(stmts) => {
                // If the sequence is empty, push a `null`
                // (Empty sequences would otherwise cause stack imbalance, since they push nothing
                //  overall)
                if stmts.is_empty() {
                    return Ok(vec![InstructionKind::Push(Literal::Null).with_loc(&loc)].into());
                }

                // Compile with interspersed `pop`s
                let mut instructions = InstructionBlock::new();
                let mut is_first_stmt = true;
                for stmt in stmts {
                    if !is_first_stmt {
                        instructions.push(InstructionKind::Pop.with_loc(&loc));
                    } else {
                        is_first_stmt = false;
                    }
                    instructions.extend(compile(stmt)?);
                }
                instructions
            },

            NodeKind::Block { body, parameters, captures } =>
                vec![InstructionKind::PushBlock {
                    parameters,
                    captures,
                    body: compile(*body)?.rc(),
                }.with_loc(&loc)].into(),

            NodeKind::EnumVariant { enum_type, variant_name, components } => {
                let mut instructions = InstructionBlock::new();

                let mut labels = vec![];
                match components {
                    SendMessageComponents::Blank => (), // No labels, nothing to do
                    SendMessageComponents::Parameterised(params) => {
                        for (name, param) in params {
                            labels.push(name);
                            match param {
                                SendMessageParameter::CallArgument(n) => instructions.extend(compile(*n)?),
                                SendMessageParameter::DefinitionParameter(_) => unreachable!(),
                            }
                        }
                    },
                    SendMessageComponents::Unary(_) => unreachable!("enum constructor cannot be unary"),
                }
                instructions.extend(compile(*enum_type)?);
                instructions.push(InstructionKind::NewVariant { name: variant_name, labels }.with_loc(&loc));
                instructions
            },

            NodeKind::Assignment { target, value } => {
                // What are we assigning to?
                match target.kind {
                    // Local variable
                    NodeKind::Identifier(i) => {
                        let mut instructions = compile(*value)?;
                        instructions.push(InstructionKind::Set(i).with_loc(&loc));
                        instructions
                    },

                    // Self
                    NodeKind::SelfAccess => {
                        let mut instructions = compile(*value)?;
                        instructions.push(InstructionKind::SetSelf.with_loc(&loc));
                        instructions
                    },

                    // Field
                    NodeKind::SendMessage { receiver, components: SendMessageComponents::Unary(field_name) } => {
                        let mut instructions = compile(*value)?;
                        instructions.extend(compile(*receiver)?);
                        instructions.push(InstructionKind::SetField(field_name).with_loc(&loc));
                        instructions
                    },

                    _ => {
                        let err: InterpreterError = InterpreterErrorKind::InvalidAssignmentTarget.into();
                        let err = err.add_details(&loc, None);
                        return Err(err)
                    }
                }
            }

            NodeKind::SendMessage { receiver, components } => {
                let mut instructions = compile(*receiver)?;

                let name = components.to_method_name();
                match components {
                    SendMessageComponents::Parameterised(args) => {
                        for (_, arg) in args {
                            match arg {
                                SendMessageParameter::CallArgument(n) => instructions.extend(compile(*n)?),
                                SendMessageParameter::DefinitionParameter(_) => unreachable!(),
                            }
                        }
                    },
                    
                    SendMessageComponents::Unary(_) => (),
                    SendMessageComponents::Blank => unreachable!("method call cannot be blank"),
                }

                let arity = name.chars().filter(|c| *c == ':').count();
                instructions.push(InstructionKind::Call { name, arity }.with_loc(&loc));
                instructions
            }

            NodeKind::ImplBlock { target, body } => {
                let inner = compile(*body)?;

                let mut instructions = compile(*target)?;
                instructions.push(InstructionKind::PushBlock {
                    parameters: BlockParameters::Named(vec![]),
                    captures: vec![],
                    body: inner.rc(),
                }.with_loc(&loc));
                instructions.push(InstructionKind::Impl.with_loc(&loc));
                instructions
            },

            NodeKind::FuncDefinition { parameters, body, is_static, documentation, visibility } => {
                let inner = compile(*body)?;
                let name = parameters.to_method_name();

                vec![
                    InstructionKind::PushBlock {
                        parameters: match parameters {
                            SendMessageComponents::Parameterised(p) => BlockParameters::Named(p.into_iter().map(|(_, n)| match n {
                                SendMessageParameter::CallArgument(_) => unreachable!(),
                                SendMessageParameter::DefinitionParameter(n) => n,
                            }).collect()),
                            SendMessageComponents::Unary(_) => BlockParameters::Named(vec![]),
                            SendMessageComponents::Blank => unreachable!("cannot define function with blank parameters"),
                        },
                        captures: vec![],
                        body: inner.rc(),
                    }.with_loc(&loc),
                    InstructionKind::DefFunc {
                        name,
                        locality: if is_static { MethodLocality::Static } else { MethodLocality::Instance },
                        documentation,
                        visibility,
                    }.with_loc(&loc),
                ].into()
            }
            
            NodeKind::EnumDefinition { name, variants, documentation } => 
                vec![InstructionKind::DefType {
                    name,
                    data: TypeData::Variants(variants),
                    documentation,
                }.with_loc(&loc)].into(),
            NodeKind::StructDefinition { name, instance_fields, static_fields, documentation } =>
                vec![InstructionKind::DefType {
                    name,
                    data: TypeData::Fields {instance_fields, static_fields },
                    documentation,
                }.with_loc(&loc)].into(),
            NodeKind::MixinDefinition { name, documentation } =>
                vec![InstructionKind::DefType {
                    name,
                    data: TypeData::Mixin,
                    documentation
                }.with_loc(&loc)].into(),
            NodeKind::Use { mixin, .. } => {
                let mut instructions = compile(*mixin)?;
                instructions.push(InstructionKind::Use.with_loc(&loc));
                instructions
            },

            NodeKind::Sugar(_) => unreachable!("sugar nodes should be expanded before compilation"),
        }
    )
}


fn indent(s: String) -> String {
    s.split('\n').map(|l| format!("  {l}")).collect::<Vec<_>>().join("\n")
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            InstructionKind::Get(n) => write!(f, "get {n}"),
            InstructionKind::GetSelf => write!(f, "get (self)"),
            InstructionKind::Set(n) => write!(f, "set {n}"),
            InstructionKind::SetField(n) => write!(f, "set field {n}"),
            InstructionKind::SetSelf => write!(f, "set (self)"),
            InstructionKind::Pop => write!(f, "pop"),
            InstructionKind::Push(l) => write!(f, "push {l}"),
            InstructionKind::PushBlock { parameters, captures: _, body } => {
                let prefix;
                let params;
                match parameters {
                    BlockParameters::Named(names) => {
                        prefix = "";
                        params = names.clone();
                    },
                    BlockParameters::All(name) => {
                        prefix = "";
                        params = vec![format!("*{name}")];
                    }
                    BlockParameters::Patterned { patterns, fatal } => {
                        // TODO: proper pattern formatting
                        if *fatal { prefix = "!" } else { prefix = "?" };
                        params = patterns.iter().map(|x| "<pattern>".into()).collect();
                    },
                };

                write!(f, "push block {}[ | {} |\n{}\n]", prefix, params.join(" "), indent(body.to_string()))
            },
            InstructionKind::Duplicate => write!(f, "dup"),
            InstructionKind::Call { name, arity: _ } => write!(f, "call {name}"),
            InstructionKind::NewVariant { name, labels } =>
                write!(f, "new variant {} {}",
                    name,
                    if labels.is_empty() { "".into() } else { format!("{}:", labels.join(":")) }
                ),
            InstructionKind::Impl => write!(f, "impl"),
            InstructionKind::DefType { name, data: _, documentation: _ } =>
                write!(f, "def type {name} ..."), // TODO: type data
            InstructionKind::Use => write!(f, "use"),
            InstructionKind::DefFunc { name, locality, documentation: _, visibility: _ } =>
                write!(f, "def func {} {} ...", name, match locality {
                    MethodLocality::Instance => "(instance)",
                    MethodLocality::Static => "(static)",
                }), // TODO: documentation
        }
    }
}


impl Display for InstructionBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in self {
            writeln!(f, "{i}")?;
        }

        Ok(())
    }
}