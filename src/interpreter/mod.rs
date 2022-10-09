use std::{fmt::Display, rc::Rc};

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
mod tests;

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub context: StackFrameContext,
    self_value: ValueRef,
    locals: Vec<(String, ValueRef)>,
}

#[derive(Debug, Clone)]
pub enum StackFrameContext {
    Root,
    Impl(TypeRef),
    Method {
        method: MethodRef,
        receiver: ValueRef,
    },
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

pub struct Interpreter {
    types: Vec<TypeRef>,
    stack: Vec<StackFrame>,
}

pub type InterpreterResult = Result<ValueRef, InterpreterError>;

impl Interpreter {
    pub fn new() -> Self {
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
        stdlib::instantiate(&mut result);
        result
    }

    pub fn parse_and_evaluate(&mut self, source_file: Rc<SourceFile>) -> InterpreterResult {
        let tokens = Tokenizer::tokenize(source_file.clone()).expect("tokenization failed");
        let node = Parser::parse_and_analyse(source_file.clone(), &tokens[..]).expect("parsing failed");
        self.evaluate(&node)
    }

    pub fn evaluate(&mut self, node: &Node) -> InterpreterResult {
        self.evaluate_inner(node).map_err(|e| e.add_details(node, self))
    }

    fn evaluate_inner(&mut self, node: &Node) -> InterpreterResult {
        match &node.kind {
            NodeKind::IntegerLiteral(i) => (*i).try_into()
                .map(|i| Value::new_integer(i).rc())
                .map_err(|_| InterpreterErrorKind::IntegerOverflow.into()),

            NodeKind::StringLiteral(s) => Ok(Value::new_string(s).rc()),

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
                    let value = value.borrow().value_copy().rc();

                    if let Some(target) = self.find_local(&id) {
                        *target.borrow_mut() = value.borrow().clone();
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
                    // Evaluate the receiver
                    let target_value = self.evaluate(&*receiver)?;

                    // Check if the receiver has a field with the correct name
                    let TypeInstance::Fields { source_type, variant, field_values } = &mut target_value.borrow_mut().type_instance else {
                        return Err(InterpreterErrorKind::InvalidAssignmentTarget.into());
                    };
                    let fields = match source_type.borrow().data {
                        TypeData::Fields(ref f) => f.clone(),
                        TypeData::Variants(ref v) => v[variant.unwrap()].fields.clone(),
                        _ => return Err(InterpreterErrorKind::InvalidAssignmentTarget.into()),
                    };
                    let field_index = fields.iter()
                        .enumerate()
                        .find(|(_, x)| x == &field_name).map(|(i, _)| i)
                        .ok_or(InterpreterErrorKind::InvalidAssignmentTarget.into())?;

                    // Assign to field
                    let value = self.evaluate(value)?;
                    field_values[field_index] = value.clone();

                    Ok(value)
                } else {
                    Err(InterpreterErrorKind::InvalidAssignmentTarget.into())
                }
            },

            NodeKind::Identifier(id) => {
                if let Some(value) = self.find_local(id) {
                    Ok(value)
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
                            .map(|v| (name.clone(), v))
                    )
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Value::new_block(Block {
                    body: *body.clone(),
                    parameters: parameters.clone(),
                    captures: capture_values,
                }).rc())
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
                    SendMessageComponents::Unary(_) => vec![],
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

            NodeKind::TrueLiteral => Ok(Value::new_boolean(self, true).rc()),
            NodeKind::FalseLiteral => Ok(Value::new_boolean(self, false).rc()),
            NodeKind::NullLiteral => Ok(Value::new_null().rc()),
            NodeKind::SelfLiteral => Ok(self.current_stack_frame().self_value.clone()),

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

            NodeKind::FuncDefinition { parameters, body, is_static } => {
                // The current stack frame should represent an `impl` block, so we know where to
                // put this method
                let StackFrame { context: StackFrameContext::Impl(t), .. } = self.current_stack_frame() else {
                    return Err(InterpreterErrorKind::FuncDefinitionInvalidContext.into());
                };
                let body = body.clone();

                let name = parameters.to_method_name();
                let internal_names = parameters.defined_internal_names();
                
                let method = Method::new_parsed(&name, *body, internal_names).rc();
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

            NodeKind::StructDefinition { name, fields } => {
                if self.resolve_type(name).is_some() {
                    return Err(InterpreterErrorKind::DuplicateTypeDefinition(name.into()).into());
                }

                let mut t = Type {
                    data: TypeData::Fields(fields.clone()),
                    ..Type::new(name)
                };
                t.generate_accessor_methods();
                mixin_derive::derive_core_mixins(self, &mut t);
                let t = t.rc();
                Type::generate_struct_constructor(t.clone());
                self.types.push(t);

                Ok(Value::new_null().rc())
            }
        }
    }

    pub fn resolve_type(&self, id: &str) -> Option<TypeRef> {
        self.types.iter().find(|t| t.borrow().id == id).cloned()
    }

    pub fn resolve_stdlib_type(&self, id: &str) -> TypeRef {
        self.resolve_type(id).unwrap_or_else(|| panic!("internal error: stdlib type {} missing", id))
    }

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

    pub fn current_stack_frame(&self) -> &StackFrame {
        self.stack.last().unwrap()
    }

    pub fn current_stack_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.last_mut().unwrap()
    }

    pub fn find_local(&self, name: &str) -> Option<ValueRef> {
        self.current_stack_frame()
            .locals
            .iter()
            .find_map(|(n, v)| if n == name { Some(v.clone()) } else { None })
    }

    pub fn create_local(&mut self, name: &str, value: ValueRef) {
        self.current_stack_frame_mut().locals.push((name.into(), value))
    }
}
