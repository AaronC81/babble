//! Implements the main Babble interpreter.
//! 
//! Babble does not use bytecode - instead, nodes are executed using their parse tree and some 
//! surrounding context. This is pretty slow, but works well enough!

use std::{fmt::Display, rc::Rc, cell::RefCell};

use crate::{parser::{Parser, BlockParameters, MethodVisibility}, tokenizer::Tokenizer, source::SourceFile};

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

mod doc;
pub use doc::*;

use self::instruction::{compile, InstructionBlock, InstructionKind, Instruction};

pub mod stdlib;
pub mod mixin_derive;

pub mod instruction;

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
    source_file: Option<Rc<SourceFile>>,
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
                write!(f, "impl block on `{}`", (**t).borrow().id),
            StackFrameContext::Method { method, receiver } =>
                write!(f, "method `{}` on `{}`", method.name, (**receiver).borrow().to_language_string() ),
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
    pub root_source_file: Option<Rc<SourceFile>>,
}

pub type InterpreterResult = Result<ValueRef, InterpreterError>;

impl Interpreter {
    /// Creates a new interpreter, with a stack containing only a root frame, and an instance of
    /// the standard library in its type repository.
    pub fn new(root_source_file: Option<Rc<SourceFile>>) -> Result<Self, InterpreterError> {
        let mut result = Self {
            types: vec![],
            stack: vec![
                StackFrame {
                    context: StackFrameContext::Root,
                    self_value: Value::new_null().rc(),
                    locals: vec![],
                    source_file: root_source_file.clone(),
                }
            ],
            root_source_file
        };
        stdlib::instantiate(&mut result)?;
        Ok(result)
    }

    /// Tokenize, parse, and compile the root source file, and then evaluate it within this
    /// interpreter.
    pub fn parse_and_evaluate_root(&mut self) -> InterpreterResult {
        self.parse_and_evaluate(self.root_source_file.clone().expect("no root source file"))
    }

    /// Tokenize, parse, and compile a source file, and then evaluate it within this interpreter.
    pub fn parse_and_evaluate(&mut self, source_file: Rc<SourceFile>) -> InterpreterResult {
        let tokens = Tokenizer::tokenize(source_file.clone())
            .map_err(|e| InterpreterErrorKind::TokenizerError(e).into())?;
        let node = Parser::parse_and_analyse(source_file, &tokens[..])
            .map_err(|e| InterpreterErrorKind::ParserError(e).into())?;
        let compiled = compile(node)?;
        self.evaluate(&compiled)
    }

    /// Evaluate a single instruction block within this interpreter.
    fn evaluate(&mut self, instructions: &InstructionBlock) -> InterpreterResult {
        let mut value_stack = vec![];

        for instruction in instructions {
            if let Err(e) = self.evaluate_inner(instruction, &mut value_stack) {
                return Err(e.add_details(&instruction.location, Some(self)));
            }
        }

        if value_stack.len() == 1 {
            Ok(value_stack.pop().unwrap())
        } else {
            let mut error: InterpreterError = InterpreterErrorKind::StackImbalance(value_stack.len()).into();
            if !instructions.as_vec().is_empty() {
                error = error.add_details(&instructions.iter().next().unwrap().location, Some(self));
            }
            Err(error)
        }
    }

    /// Evaluate a single instruction within this interpreter, modifying the given stack of values.
    #[inline(always)]
    fn evaluate_inner(&mut self, instruction: &Instruction, value_stack: &mut Vec<ValueRef>) -> Result<(), InterpreterError> {
        match &instruction.kind {
            InstructionKind::Get(id) => {
                // Push value of local
                if let Some(var) = self.find_local(id) {
                    value_stack.push(var.borrow().value.clone());
                    Ok(())
                } else if let Some(t) = self.resolve_type(id) {
                    value_stack.push(Value::new_type(t).rc());
                    Ok(())
                } else {
                    return Err(InterpreterErrorKind::MissingName(id.into()).into())
                }
            },
            
            InstructionKind::GetSelf => {
                // Push `self` from current frame
                value_stack.push(self.current_stack_frame().self_value.clone());
                Ok(())
            }

            InstructionKind::Set(id) => {
                // Peek the value to assign
                //
                // Perform a value copy so that we don't actually mirror the value of the 
                // assignment value, for example:
                //   a = 3.
                //   b = 5.
                //   a = 10. // b should remain 5
                let value = value_stack.last().unwrap().clone();
                let value = Value::soft_copy(value);

                if let Some(target) = self.find_local(id) {
                    target.borrow_mut().value = value;
                } else {
                    self.create_local(id, value);
                }
                Ok(())
            },

            InstructionKind::SetSelf => {
                // Peek value and overwrite `self`
                let value = value_stack.last().unwrap().clone();
                let value = Value::soft_copy(value);
                *self.stack.last_mut().unwrap().self_value.borrow_mut() = value.borrow().clone();
                Ok(())
            },

            InstructionKind::Pop => {
                // Pop top value and discard
                value_stack.pop().unwrap();
                Ok(())
            },

            InstructionKind::Duplicate => {
                // Peek value and push it again
                value_stack.push(value_stack.last().unwrap().clone());
                Ok(())
            },

            InstructionKind::Push(l) => {
                // Instantiate literal and push it
                value_stack.push(Value::from_literal(l, self)?);
                Ok(())
            },

            // These more-expensive instructions are broken out into their own handlers.
            //
            // This is because, in the generated LLVM IR, rustc `alloca`'s EVERY SINGLE local
            // variable, for every single match case, even though they're mutually exclusive.
            // This means that calls to `evaluate_inner` consume an absolutely ludicrous amount of
            // stack (several kB), causing us to overflow very quickly when compiling without
            // optimisations.
            // 
            // There's evidently an optimisation which resolves this in release mode, because it's
            // nowhere near as much of a problem there. This actually produces *slightly worse*
            // results in release mode, but I think it's worth it.
            //
            // Here's how this optimisation influences the number of Babble recursions before
            // overflow (rustc 1.71.0-nightly c609da59d 2023-04-18, Apple M1 Pro, ulimit -s = 8176):
            //   
            //             Without   With
            //   - Debug:    344  -> 717
            //   - Release:  4136 -> 3832
            //
            InstructionKind::SetField   (_)    => self.instruction_set_field  (&instruction.kind, value_stack),
            InstructionKind::PushBlock  { .. } => self.instruction_push_block (&instruction.kind, value_stack),
            InstructionKind::Call       { .. } => self.instruction_call       (&instruction.kind, value_stack),
            InstructionKind::NewVariant { .. } => self.instruction_new_variant(&instruction.kind, value_stack),
            InstructionKind::Impl              => self.instruction_impl       (&instruction.kind, value_stack),
            InstructionKind::Use        { .. } => self.instruction_use        (&instruction.kind, value_stack),
            InstructionKind::DefType    { .. } => self.instruction_def_type   (&instruction.kind, value_stack),
            InstructionKind::DefFunc    { .. } => self.instruction_def_func   (&instruction.kind, value_stack),
        }
    }

    fn instruction_set_field(&mut self, instruction_kind: &InstructionKind, value_stack: &mut Vec<ValueRef>) -> Result<(), InterpreterError> {
        let InstructionKind::SetField(field_name) = instruction_kind else { unreachable!() };

        // Pop target, then peek value
        let target_value = value_stack.pop().unwrap();
        let value = value_stack.last().unwrap().clone();

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
        
        field_values[field_index] = value;

        Ok(())        
    }

    fn instruction_push_block(&mut self, instruction_kind: &InstructionKind, value_stack: &mut Vec<ValueRef>) -> Result<(), InterpreterError> {
        let InstructionKind::PushBlock { parameters, captures, body } = instruction_kind else { unreachable!() };

        // Grab captures from local variables
        let capture_values = captures
        .iter()
        .map(|name|
            self.find_local(name)
                .ok_or_else(|| InterpreterErrorKind::MissingCaptureName(name.clone()).into())
        )
        .collect::<Result<Vec<_>, _>>()?;

        // Build block and push it
        let block = Value::new_block(Block::new(
            body.clone(),
            parameters.clone(),
            capture_values,
            self.current_stack_frame().self_value.clone(),
        )).rc();
        value_stack.push(block);

        Ok(())
    }

    fn instruction_call(&mut self, instruction_kind: &InstructionKind, value_stack: &mut Vec<ValueRef>) -> Result<(), InterpreterError> {
        let InstructionKind::Call { name, arity } = instruction_kind else { unreachable!() };

        // Pop arguments
        let mut args = vec![];
        for _ in 0..*arity {
            args.insert(0, value_stack.pop().unwrap());
        }

        // Pop receiver
        let receiver = value_stack.pop().unwrap();

        // Perform method call and push result - handle magic if needed
        let mut result = self.send_message(receiver.clone(), name, &args[..]);
        if let Err(InterpreterError { kind: InterpreterErrorKind::Magic, .. }) = result {
            result = self.handle_magic(receiver, name, &args[..]);
        }
        value_stack.push(result?);

        Ok(())
    }

    fn instruction_new_variant(&mut self, instruction_kind: &InstructionKind, value_stack: &mut Vec<ValueRef>) -> Result<(), InterpreterError> {
        let InstructionKind::NewVariant { name, labels } = instruction_kind else { unreachable!() };

        // Pop the type, and get its variants
        let enum_type =
        if let TypeInstance::Type(t) = &value_stack.pop().unwrap().borrow().type_instance {
            t.clone()
        } else {
            // Should never parse
            unreachable!()
        };

        // Find details of the requested variant
        let enum_type_ref = enum_type.borrow();
        let (variant_idx, variant) = enum_type_ref.resolve_variant(name)?;

        // Check that names of passed fields match expected fields
        if labels != &variant.fields {
            return Err(InterpreterErrorKind::IncorrectVariantParameters.into())
        }

        // Pop each field value
        let mut field_values = vec![];
        for _ in 0..labels.len() {
            field_values.insert(0, value_stack.pop().unwrap());
        }

        // Instantiate enum variant and push it
        value_stack.push(Value {
            type_instance: TypeInstance::Fields {
                source_type: enum_type.clone(),
                variant: Some(variant_idx),
                field_values,
            }
        }.rc());

        Ok(())
    }

    fn instruction_impl(&mut self, instruction_kind: &InstructionKind, value_stack: &mut Vec<ValueRef>) -> Result<(), InterpreterError> {
        let InstructionKind::Impl = instruction_kind else { unreachable!() };

        // Pop body (should be a block), and peek target (should be a type)
        let body_v = value_stack.pop().unwrap();
        let body_b = body_v.borrow();
        let body = body_b.to_block()?;
        let target = value_stack.last().unwrap().borrow().to_type()?;

        // Push new stack frame and evaluate body inside it
        self.stack.push(StackFrame {
            context: StackFrameContext::Impl(target.clone()),
            self_value: Value::new_type(target).rc(),
            locals: vec![],
            source_file: Some(body.body.source_file()),
        });
        self.evaluate(&body.body)?;
        self.stack.pop();

        Ok(())
    }

    fn instruction_use(&mut self, instruction_kind: &InstructionKind, value_stack: &mut Vec<ValueRef>) -> Result<(), InterpreterError> {
        let InstructionKind::Use { is_static } = instruction_kind else { unreachable!() };

        // The current stack frame should represent an `impl` block, so we know where to
        // use this mixin
        let StackFrame { context: StackFrameContext::Impl(t), .. } = self.current_stack_frame() else {
            return Err(InterpreterErrorKind::UseInvalidContext.into());
        };
        let t = t.clone();
        
        // Peek the stack, it should be a mixin type
        let mixin = value_stack.last().unwrap().clone().borrow().to_type()?;
        let TypeData::Mixin = mixin.borrow().data else {
            return Err(InterpreterErrorKind::UseNonMixin(mixin.borrow().id.clone()).into());
        };

        // Insert used mixin
        if *is_static {
            t.borrow_mut().used_static_mixins.push(mixin);
        } else {
            t.borrow_mut().used_mixins.push(mixin);
        }
        
        Ok(())
    }

    fn instruction_def_type(&mut self, instruction_kind: &InstructionKind, value_stack: &mut Vec<ValueRef>) -> Result<(), InterpreterError> {
        let InstructionKind::DefType { name, data, documentation } = instruction_kind else { unreachable!() };

        // Check no equivalently-named type already exists
        if self.resolve_type(name).is_some() {
            return Err(InterpreterErrorKind::DuplicateTypeDefinition(name.into()).into());
        }
        let (is_struct, is_enum) = match data {
            TypeData::Fields { .. } => (true, false),
            TypeData::Variants(_) => (false, true),
            _ => (false, false),
        };

        // Create new type
        let mut t = Type {
            data: data.clone(),
            documentation: match documentation {
                Some(d) => DocumentationState::Documented(d.into()),
                None => DocumentationState::Undocumented,
            },
            ..Type::new(name)
        };

        // Structs or enums require accessors core mixin derivation
        if is_struct || is_enum {
            t.generate_accessor_methods();
            mixin_derive::derive_core_mixins(self, &mut t);
        }

        // Make our type shared and available
        let t = t.rc();
        self.types.push(t.clone());

        // Structs require a constructor to be generated
        if is_struct {
            Type::generate_struct_constructor(t.clone());
        }

        // Push reference to new type
        value_stack.push(Value::new_type(t).rc());

        Ok(())
    }

    fn instruction_def_func(&mut self, instruction_kind: &InstructionKind, value_stack: &mut Vec<ValueRef>) -> Result<(), InterpreterError> {
        let InstructionKind::DefFunc { name, locality, unordered, documentation, visibility } = instruction_kind else { unreachable!() };

        // The current stack frame should represent an `impl` block, so we know where to
        // put this method
        let StackFrame { context: StackFrameContext::Impl(t), .. } = self.current_stack_frame() else {
            return Err(InterpreterErrorKind::FuncDefinitionInvalidContext.into());
        };

        // Peek the stack to get this method's body
        let block = value_stack.last().unwrap().clone().borrow().to_block()?.clone();
        let internal_names = match block.parameters {
            BlockParameters::Named(n) => n,
            BlockParameters::All(_) => unreachable!(),
        };
        
        let mut method = Method::new_compiled(name, block.body, internal_names);
        if let Some(documentation) = documentation {
            method.add_documentation(documentation);
        }
        method.visibility = *visibility;
        if *unordered {
            method = method.unordered();
        }
        let method = method.rc();
        match locality {
            MethodLocality::Instance => t.borrow_mut().add_method(method),
            MethodLocality::Static => t.borrow_mut().add_static_method(method),
        }

        Ok(())
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
        self.resolve_type(id).unwrap_or_else(|| panic!("internal error: stdlib type {id} missing"))
    }

    /// Assumes that a method is a magic method, calls it, and returns the result of the call.
    /// 
    /// If the method is not magic and must be handled normally, panics.
    pub fn handle_magic(&mut self, receiver: ValueRef, method_name: &str, args: &[ValueRef]) -> InterpreterResult {
        if let Value { type_instance: TypeInstance::Type(ref t) } = *receiver.borrow() {
            // Program filePathBacktrace
            if t.borrow().id == "Program" && method_name == "filePathBacktrace" {
                let mut paths = vec![];
                for frame in self.stack.iter().rev() {
                    if let Some(source_file) = frame.source_file.clone() {
                        paths.push(Value::new_string(&source_file.name).rc());
                    } else {
                        paths.push(Value::new_null().rc());
                    }
                }
                return Ok(Value::new_array(&paths).rc())
            }
        }

        unreachable!("unknown magic method")
    }

    /// Sends a message (i.e. calls a method), given a receiver, method name, and a set of values as
    /// arguments.
    /// 
    /// Returns an error if the method does not exist on the receiver.
    #[inline(always)]
    pub fn send_message(&mut self, receiver: ValueRef, method_name: &str, args: &[ValueRef]) -> InterpreterResult {
        let receiver_ref = receiver.borrow();

        let method =
            if let TypeInstance::Type(t) = &receiver_ref.type_instance {
                t.borrow().resolve_static_method(method_name)
            } else {
                receiver_ref.type_instance.get_type(self).borrow().resolve_instance_method(method_name)
            };

        if let Some(method) = method {
            drop(receiver_ref);

            // Make sure that we're allowed to call the method from here
            match method.visibility {
                MethodVisibility::Public => (), // Always allowed
                MethodVisibility::Private => {
                    // Check that the current self value's type is the same as the receiver's type
                    // Use `Reflection instanceType:` for this, so that we get the correct semantics
                    // for static methods
                    let reflection = Value::new_type(self.resolve_stdlib_type("Reflection")).rc();
                    let self_value = self.current_stack_frame().self_value.clone();
                    let self_type = self.send_message(
                        reflection.clone(),
                        "instanceType:",
                        &[self_value],
                    )?;
                    let receiver_type = self.send_message(
                        reflection,
                        "instanceType:",
                        &[receiver.clone()],
                    )?;

                    // If the types aren't the same, don't allow calling this
                    if self_type.borrow().to_type()? != receiver_type.borrow().to_type()? {
                        return Err(InterpreterErrorKind::PrivateMethod(receiver.clone(), method_name.into()).into())
                    }
                },
            }

            // Call the method
            // (This creates a frame if necessary)
            self.call_method(method, receiver, args)
        } else {
            Err(InterpreterErrorKind::MissingMethod(receiver.clone(), method_name.into()).into())
        }
    }

    /// Call a block, passing it a given set of arguments, and returning the result of executing
    /// the block's body.
    /// 
    /// The block is evaluated within a new stack frame, with captured locals referenced as locals
    /// within that frame. Arguments are also created as locals.
    /// 
    /// Returns an error if the number of arguments does not match the expected arity.
    #[inline(always)]
    pub fn call_block(&mut self, block: &Block, arguments: Vec<ValueRef>) -> InterpreterResult {
        if block.arity() >= 0 && block.arity() as usize != arguments.len() {
            return Err(InterpreterErrorKind::IncorrectArity {
                name: "anonymous block".into(),
                expected: arguments.len(),
                got: block.arity() as usize,
            }.into())
        }

        let parameter_locals = match &block.parameters {
            // For named parameters, simply map arguments to parameters one-to-one
            BlockParameters::Named(ref parameters) => {
                parameters.iter()
                    .cloned()
                    .zip(arguments)
                    .map(|(name, value)| LocalVariable { name, value }.rc())
                    .collect::<Vec<_>>()
            },

            // For all parameter, just use the array
            BlockParameters::All(name) => {
                vec![
                    LocalVariable {
                        name: name.into(),
                        value: Value::new_array(&arguments).rc(),
                    }.rc()
                ]
            }
        };

        // Create a new stack frame with the relevant locals - that is, parameters and captures
        self.stack.push(StackFrame {
            locals: parameter_locals
                .into_iter()
                .chain(block.captured_locals.iter().cloned())
                .collect(),
            self_value: block.captured_self.clone(),
            context: StackFrameContext::Block,
            source_file: Some(block.body.source_file()),
        });

        // Run the body, bail if it fatally errored, and then pop the stack frame
        // This order may seem unintuitive - but when an error is fatal, then we want the stack
        // trace from the error to be as correct as possible, so we leave the frames which errored
        // on the stack
        let mut result = self.evaluate(&block.body);
        if let Err(error) = &result && error.kind.is_fatal() {
            return Err(error.clone());
        }
        self.stack.pop();

        result
    }

    /// Call a method, passing a receiver and a set of arguments.
    /// 
    /// If the method is implemented from parsed nodes, the method body is evaluated within a new
    /// stack frame. Intrinsic methods do not create a stack frame. Arguments are created as locals.
    /// 
    /// Returns an error if the number of arguments does not match the expected arity.
    #[inline(always)]
    pub fn call_method(&mut self, method: Rc<Method>, receiver: ValueRef, arguments: &[ValueRef]) -> InterpreterResult {        
        if method.arity != arguments.len() {
            return Err(InterpreterErrorKind::IncorrectArity {
                name: method.name.clone(),
                expected: method.arity,
                got: arguments.len(),
            }.into())
        }

        match &method.implementation {
            // Internal methods don't need a stack frame, since we control their behaviour
            MethodImplementation::Internal(func) =>
                (func)(self, receiver, arguments),
            
            MethodImplementation::Compiled { instructions, internal_names } => {
                // Create a new stack frame with the relevant parameters
                self.stack.push(StackFrame {
                    locals: internal_names.iter()
                        .cloned()
                        .zip(arguments)
                        .map(|(name, value)| LocalVariable { name, value: value.clone() }.rc())
                        .collect(),
                    self_value: receiver.clone(),
                    context: StackFrameContext::Method {
                        method: method.clone(),
                        receiver,
                    },
                    source_file: Some(instructions.source_file()),
                });

                // Run the body, bail if it fatally errored, and then pop the stack frame
                // This order may seem unintuitive - but when an error is fatal, then we want the
                // stack trace from the error to be as correct as possible, so we leave the frames
                // which errored on the stack
                let result = self.evaluate(instructions);
                if let Err(error) = &result && error.kind.is_fatal() {
                    return Err(error.clone());
                }
                self.stack.pop();
                Ok(result?)
            },

            MethodImplementation::Magic => Err(InterpreterErrorKind::Magic.into()),

            MethodImplementation::UnorderedProxy { target, argument_order } => {
                // Reorder the arguments which we were passed to match the order of the target
                let mut reordered_arguments = vec![];
                for i in argument_order {
                    reordered_arguments.push(arguments[*i].clone());
                }

                self.call_method(target.clone(), receiver, &reordered_arguments)
            }
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
