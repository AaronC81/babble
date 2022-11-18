//! Implements the main Babble interpreter.
//! 
//! Babble does not use bytecode - instead, nodes are executed using their parse tree and some 
//! surrounding context. This is pretty slow, but works well enough!

use std::{fmt::Display, rc::Rc, cell::RefCell};

use crate::{parser::{NodeKind, Node, SendMessageComponents, Parser}, tokenizer::Tokenizer, source::SourceFile};

mod error;

pub use error::*;

mod value;
pub use value::*;

mod r#type;
pub use r#type::*;

mod block;
pub use block::*;

mod method;
pub use method::*;

pub mod stdlib;
pub mod mixin_derive;

#[cfg(test)]
pub mod tests;

/// A named local variable with a value.
#[derive(Debug)]
pub struct LocalVariable {
    name: String,
    value: ValueRef,
}

impl LocalVariable {
    /// Transforms this into a [LocalVariableRef].
    pub fn rc(self) -> LocalVariableRef {
        Rc::new(RefCell::new(self))
    }
}

pub type LocalVariableRef = Rc<RefCell<LocalVariable>>;

/// A stack frame, which provides a value for `self`, and may hold the definition (or captures, in
/// the case of a block) of a set of local variables.
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub context: StackFrameContext,
    self_value: ValueRef,
    locals: Vec<LocalVariableRef>,
}

/// A description of the context which created a stack frame.
#[derive(Debug, Clone)]
pub enum StackFrameContext {
    /// The stack frame is the root of the main source file. 
    Root,

    /// The stack frame is an `impl` block on a type.
    Impl(TypeRef),

    /// The stack frame is a call to a [`Method`].
    Method {
        method: MethodRef,
        receiver: ValueRef,
    },

    /// The stack frame is a call to a [`Block`].
    Block,
}

impl Display for StackFrameContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackFrameContext::Root =>
                f.write_str("root"),
            StackFrameContext::Impl(t) =>
                write!(f, "impl block on `{}`", (&**t).borrow().id),
            StackFrameContext::Method { method, receiver } =>
                write!(f, "method `{}` on `{}`", method.name, (&**receiver).borrow().to_language_string() ),
            StackFrameContext::Block =>
                f.write_str("block"),
        }
    }
}

/// The main interpreter context, which holds the current execution stack, and a repository of all
/// defined types.
pub struct Interpreter {
    pub types: Vec<TypeRef>,
    pub stack: Vec<StackFrame>,
}

pub type InterpreterResult = Result<ValueRef, InterpreterError>;

impl Interpreter {
    /// Creates a new interpreter, with a stack containing only a root frame, and an instance of
    /// the standard library in its type repository.
    pub fn new() -> Result<Self, InterpreterError> {
        let mut result = Self {
            types: vec![],
            stack: vec![
                StackFrame {
                    context: StackFrameContext::Root,
                    self_value: Value::new_null().rc(),
                    locals: vec![],
                }
            ],
        };
        stdlib::instantiate(&mut result)?;
        Ok(result)
    }

    /// Tokenize and parse a source file, **panicking** if this fails, and then evaluate it within
    /// this interpreter.
    pub fn parse_and_evaluate(&mut self, source_file: Rc<SourceFile>) -> InterpreterResult {
        let tokens = Tokenizer::tokenize(source_file.clone()).expect("tokenization failed");
        let node = Parser::parse_and_analyse(source_file.clone(), &tokens[..]).expect("parsing failed");
        self.evaluate(&node)
    }

    /// Evaluate a single node within this interpreter.
    pub fn evaluate(&mut self, node: &Node) -> InterpreterResult {
        self.evaluate_inner(node).map_err(|e| e.add_details(node, self))
    }

    /// The inner implementation of `evaluate`.
    fn evaluate_inner(&mut self, node: &Node) -> InterpreterResult {
        match &node.kind {
            NodeKind::SendMessage { receiver, components } => {
                // Evaluate the receiver
                let receiver = self.evaluate(receiver)?;

                self.send_message(receiver, components)
            },

            NodeKind::StatementSequence(seq) => {
                // Bodies return their last statement, except if they have no statements, in which
                // case they return null
                let mut result = Value::new_null().rc();
                for node in seq {
                    result = self.evaluate(node)?;
                }
                Ok(result)
            },

            NodeKind::Assignment { target, value } => {
                // Are we assigning to a local?
                if let box Node { kind: NodeKind::Identifier(id), .. } = target {
                    // Perform a value copy so that we don't actually mirror the value of the 
                    // assignment value, for example:
                    //   a = 3.
                    //   b = 5.
                    //   a = 10. // b should remain 5
                    let value = self.evaluate(value)?;
                    let value = Value::soft_copy(value);

                    if let Some(target) = self.find_local(&id) {
                        target.borrow_mut().value = value.clone();
                    } else {
                        self.create_local(&id, value.clone());
                    }

                    Ok(value)
                // Are we assigning to a field?
                } else if let box Node {
                    kind: NodeKind::SendMessage {
                        receiver,
                        components: SendMessageComponents::Unary(field_name)
                    },
                    ..
                } = target {
                    // Evaluate value and then receiver
                    // This order is important for code like `self value = self value add: 1`, which
                    // would otherwise hold a borrow of `self` for too long and cause the
                    // `Rc<RefCell<Value>>` to panic
                    let value = self.evaluate(value)?;
                    let target_value = self.evaluate(&*receiver)?;

                    // Check if the receiver has a field with the correct name
                    let mut target_value = target_value.borrow_mut();
                    let mut type_borrow;
                    let fields;
                    let field_values;
                    match &mut target_value.type_instance {
                        TypeInstance::Fields { source_type, variant, field_values: fv } => {
                            fields = match source_type.borrow().data {
                                TypeData::Fields { ref instance_fields, .. } =>
                                    instance_fields.clone(),
                                TypeData::Variants(ref v) => v[variant.unwrap()].fields.clone(),
                                _ => return Err(InterpreterErrorKind::InvalidAssignmentTarget.into()),
                            };
                            field_values = fv;
                        },

                        TypeInstance::Type(source_type) => {
                            fields = match source_type.borrow().data {
                                TypeData::Fields { ref static_fields, .. } =>
                                    static_fields.clone(),
                                _ => return Err(InterpreterErrorKind::InvalidAssignmentTarget.into()),
                            };
                            type_borrow = source_type.borrow_mut();
                            field_values = &mut type_borrow.static_fields;
                        },

                        _ => return Err(InterpreterErrorKind::InvalidAssignmentTarget.into()),
                    };
                    
                    let field_index = fields.iter()
                        .enumerate()
                        .find(|(_, x)| x == &field_name).map(|(i, _)| i)
                        .ok_or(InterpreterErrorKind::InvalidAssignmentTarget.into())?;
                    
                    field_values[field_index] = value.clone();

                    Ok(value)
                // Are we assigning to `self`?
                } else if let box Node { kind: NodeKind::SelfAccess, .. } = target {
                    let value = self.evaluate(value)?;
                    let value = Value::soft_copy(value);
                    *self.stack.last_mut().unwrap().self_value.borrow_mut() = value.borrow().clone();
                    Ok(value)
                } else {
                    Err(InterpreterErrorKind::InvalidAssignmentTarget.into())
                }
            },

            NodeKind::Identifier(id) => {
                if let Some(var) = self.find_local(id) {
                    Ok(var.borrow().value.clone())
                } else if let Some(t) = self.resolve_type(id) {
                    Ok(Value::new_type(t).rc())
                } else {
                    Err(InterpreterErrorKind::MissingName(id.into()).into())
                }
            },

            NodeKind::Block { body, parameters, captures } => {
                // Grab captures from local variables
                let capture_values = captures
                    .iter()
                    .map(|name|
                        self.find_local(name)
                            .ok_or_else(|| InterpreterErrorKind::MissingCaptureName(name.clone()).into())
                    )
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Value::new_block(Block::new(
                    *body.clone(),
                    parameters.clone(),
                    capture_values,
                    self.current_stack_frame().self_value.clone(),
                )).rc())
            },

            NodeKind::EnumVariant { enum_type, variant_name, components } => {
                // Resolve the type and its variants
                let enum_type =
                    if let TypeInstance::Type(t) = &self.evaluate(enum_type)?.borrow().type_instance {
                        t.clone()
                    } else {
                        // Should never parse
                        unreachable!()
                    };

                // Find details of the requested variant
                let enum_type_ref = enum_type.borrow();
                let (variant_idx, variant) = enum_type_ref.resolve_variant(variant_name)?;

                // Check that names of passed fields match expected fields
                let given_field_names = match components {
                    SendMessageComponents::Blank => vec![],
                    SendMessageComponents::Unary(_) => unreachable!(),
                    SendMessageComponents::Parameterised(params) => params.iter().map(|(n, _)| n.clone()).collect()
                };
                if given_field_names != variant.fields {
                    return Err(InterpreterErrorKind::IncorrectVariantParameters.into())
                }

                // Evaluate and value-copy fields
                let field_values = components.evaluate_parameters(self)?;

                Ok(Value {
                    type_instance: TypeInstance::Fields {
                        source_type: enum_type.clone(),
                        variant: Some(variant_idx),
                        field_values,
                    }
                }.rc())
            },

            NodeKind::Literal(l) => l.instantiate(self),
            NodeKind::SelfAccess => Ok(self.current_stack_frame().self_value.clone()),

            NodeKind::ImplBlock { target, body } => {
                // Evaluate target - it should be a type
                let target = self.evaluate(target)?.borrow().to_type()?;

                // Push new stack frame and evaluate body inside it
                self.stack.push(StackFrame {
                    context: StackFrameContext::Impl(target),
                    self_value: Value::new_null().rc(),
                    locals: vec![],
                });
                self.evaluate(body)?;
                self.stack.pop();

                Ok(Value::new_null().rc())
            },

            NodeKind::Use(mixin) => {
                // The current stack frame should represent an `impl` block, so we know where to
                // use this mixin
                let StackFrame { context: StackFrameContext::Impl(t), .. } = self.current_stack_frame() else {
                    return Err(InterpreterErrorKind::UseInvalidContext.into());
                };
                let t = t.clone();
                
                // The given target node should resolve to a mixin type
                let mixin = self.evaluate(mixin)?.borrow().to_type()?;
                let TypeData::Mixin = mixin.borrow().data else {
                    return Err(InterpreterErrorKind::UseNonMixin(mixin.borrow().id.clone()).into());
                };

                // Insert used mixin
                t.borrow_mut().used_mixins.push(mixin);

                Ok(Value::new_null().rc())
            },

            NodeKind::MixinDefinition { name } => {
                self.types.push(Type {
                    data: TypeData::Mixin,
                    ..Type::new(name)
                }.rc());

                Ok(Value::new_null().rc())
            },

            NodeKind::FuncDefinition { parameters, body, is_static, documentation } => {
                // The current stack frame should represent an `impl` block, so we know where to
                // put this method
                let StackFrame { context: StackFrameContext::Impl(t), .. } = self.current_stack_frame() else {
                    return Err(InterpreterErrorKind::FuncDefinitionInvalidContext.into());
                };
                let body = body.clone();

                let name = parameters.to_method_name();
                let internal_names = parameters.defined_internal_names();
                
                let mut method = Method::new_parsed(&name, *body, internal_names);
                if let Some(documentation) = documentation {
                    method.add_documentation(documentation);
                }
                let method = method.rc();
                if *is_static {
                    t.borrow_mut().add_static_method(method);
                } else {
                    t.borrow_mut().add_method(method);
                }

                Ok(Value::new_null().rc())
            },

            NodeKind::EnumDefinition { name, variants } => {
                if self.resolve_type(name).is_some() {
                    return Err(InterpreterErrorKind::DuplicateTypeDefinition(name.into()).into());
                }

                let mut t = Type {
                    data: TypeData::Variants(variants.clone()),
                    ..Type::new(name)
                };
                t.generate_accessor_methods();
                mixin_derive::derive_core_mixins(self, &mut t);
                self.types.push(t.rc());

                Ok(Value::new_null().rc())
            },

            NodeKind::StructDefinition { name, instance_fields, static_fields } => {
                if self.resolve_type(name).is_some() {
                    return Err(InterpreterErrorKind::DuplicateTypeDefinition(name.into()).into());
                }

                let mut t = Type {
                    data: TypeData::Fields {
                        instance_fields: instance_fields.clone(),
                        static_fields: static_fields.clone(),
                    },
                    ..Type::new(name)
                };
                t.generate_accessor_methods();
                mixin_derive::derive_core_mixins(self, &mut t);
                let t = t.rc();
                Type::generate_struct_constructor(t.clone());
                self.types.push(t);

                Ok(Value::new_null().rc())
            },

            NodeKind::Sugar(_) => unreachable!("sugar found by interpreter"),
        }
    }

    /// Retrieve a type by name, or return `None` if it does not exist.
    pub fn resolve_type(&self, id: &str) -> Option<TypeRef> {
        self.types.iter().find(|t| t.borrow().id == id).cloned()
    }

    /// A stricter strategy for resolving types, which instead panics if the type does not exist.
    /// 
    /// Should only be used for types which are guaranteed to exist under normal circumstances, such
    /// as `Boolean`.
    pub fn resolve_stdlib_type(&self, id: &str) -> TypeRef {
        self.resolve_type(id).unwrap_or_else(|| panic!("internal error: stdlib type {} missing", id))
    }

    /// Sends a message (i.e. calls a method), given a receiver and a set of parameters with values
    /// (named the _components_ of the message).
    /// 
    /// Returns an error if the method does not exist on the receiver.
    pub fn send_message(&mut self, receiver: ValueRef, components: &SendMessageComponents) -> InterpreterResult {
        let receiver_ref = receiver.borrow();

        let method_name = components.to_method_name();
        let method =
            if let TypeInstance::Type(t) = &receiver_ref.type_instance {
                t.borrow().resolve_static_method(&method_name)
            } else {
                receiver_ref.type_instance.get_type(self).borrow().resolve_instance_method(&method_name)
            };
        if let Some(method) = method {
            drop(receiver_ref);

            // Evaluate and value-copy parameters
            let parameters = components.evaluate_parameters(self)?;

            // Call the method
            // (This creates a frame if necessary)
            method.call(self, receiver, parameters)
        } else {
            Err(InterpreterErrorKind::MissingMethod(receiver.clone(), method_name).into())
        }
    }

    /// Returns a reference to the current top-most stack frame.
    pub fn current_stack_frame(&self) -> &StackFrame {
        self.stack.last().unwrap()
    }

    /// Returns a mutable reference to the current top-most stack frame.
    pub fn current_stack_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.last_mut().unwrap()
    }

    /// Finds a local variable within the current stack frame, and returns a reference to it if it
    /// exists, or `None` if it doesn't.
    /// 
    /// Babble's only additional form of scoping within methods are blocks, which will capture
    /// locals when necessary and bring them into the current frame's locals. As such, this does not
    /// recurse into deeper frames than the current top-most one.
    pub fn find_local(&self, name: &str) -> Option<LocalVariableRef> {
        self.current_stack_frame()
            .locals
            .iter()
            .find(|l| l.borrow().name == name)
            .cloned()
    }

    /// Creates a new local variable on the current top-most stack frame.
    pub fn create_local(&mut self, name: &str, value: ValueRef) {
        self.current_stack_frame_mut().locals.push(LocalVariable {
            name: name.into(),
            value,
        }.rc())
    }
}
