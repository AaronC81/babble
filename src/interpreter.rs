use std::{rc::Rc, fmt::Debug, cell::RefCell};
use crate::{parser::{Node, NodeKind}, source::Location, stdlib};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub type_instance: TypeInstance,
}

pub type ValueRef = Rc<RefCell<Value>>;

impl Value {
    pub fn new_integer(value: i64) -> Self {
        Self { type_instance: TypeInstance::PrimitiveInteger(value) }
    }

    pub fn new_type(t: Rc<Type>) -> Self {
        Self { type_instance: TypeInstance::Type(t) }
    }

    pub fn new_null() -> Self {
        Self { type_instance: TypeInstance::PrimitiveNull }
    }

    pub fn rc(self) -> ValueRef {
        Rc::new(RefCell::new(self))
    }

    pub fn to_integer(&self) -> Result<i64, InterpreterError> {
        if let TypeInstance::PrimitiveInteger(i) = self.type_instance {
            Ok(i)
        } else {
            Err(InterpreterError::IncorrectType)
        }
    }

    pub fn to_language_string(&self) -> String {
        match &self.type_instance {
            TypeInstance::Fields { source_type, field_values } => {
                let mut result = source_type.id.clone();
                for (field, value) in source_type.fields.iter().zip(field_values.iter()) {
                    result.push(' ');
                    result.push_str(field);
                    result.push_str(": ");

                    // If the value contains any whitespace, wrap it in parentheses
                    let mut value_str = value.to_language_string();
                    if value_str.chars().any(|c| c.is_whitespace()) {
                        value_str.insert(0, '(');
                        value_str.push(')');
                    }
                    result.push_str(&value_str);
                }

                result
            }

            TypeInstance::Type(t) => t.id.clone(),
            TypeInstance::PrimitiveInteger(i) => i.to_string(),
            TypeInstance::PrimitiveNull => "null".into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInstance {
    Fields {
        source_type: Rc<Type>,
        field_values: Vec<Box<Value>>,
    },
    Type(Rc<Type>),
    PrimitiveInteger(i64),
    PrimitiveNull,
}

impl TypeInstance {
    pub fn get_type(&self, interpreter: &Interpreter) -> Rc<Type> {
        match self {
            TypeInstance::Fields { source_type, .. } => source_type.clone(),
            TypeInstance::Type(_) => interpreter.resolve_stdlib_type("Null"), // TODO: should probably be a `Type` type
            TypeInstance::PrimitiveInteger(_) => interpreter.resolve_stdlib_type("Integer"),
            TypeInstance::PrimitiveNull => interpreter.resolve_stdlib_type("Null"),
        }
    }
}

#[derive(Debug)]
pub struct Type {
    pub id: String,
    pub fields: Vec<String>,
    pub methods: Vec<InternalMethodRef>,
    pub static_methods: Vec<InternalMethodRef>,
}
impl PartialEq for Type { fn eq(&self, other: &Self) -> bool { self.id == other.id } }
impl Eq for Type {}

impl Type {
    pub fn new(id: &str) -> Self {
        Self {
            id: id.into(),
            fields: vec![],
            methods: vec![],
            static_methods: vec![],
        }
    }

    pub fn resolve_method(&self, name: &str) -> Option<InternalMethodRef> {
        self.methods.iter().find(|m| m.name == name).cloned()
    }

    pub fn resolve_static_method(&self, name: &str) -> Option<InternalMethodRef> {
        self.static_methods.iter().find(|m| m.name == name).cloned()
    }
}

pub struct InternalMethod {
    pub name: String,
    function: Box<dyn Fn(ValueRef, Vec<ValueRef>) -> InterpreterResult>,
}
impl Debug for InternalMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InternalMethod").field("name", &self.name).finish()
    }
}

type InternalMethodRef = Rc<InternalMethod>;

impl InternalMethod {
    pub fn new<F>(name: &str, function: F) -> Self
    where F: Fn(ValueRef, Vec<ValueRef>) -> InterpreterResult + 'static {
        Self {
            name: name.into(),
            function: Box::new(function),
        }
    }
    
    pub fn rc(self) -> InternalMethodRef {
        Rc::new(self)
    }

    pub fn arity(&self) -> usize {
        self.name.matches(":").count()
    }

    pub fn call(&self, receiver: ValueRef, parameters: Vec<ValueRef>) -> InterpreterResult {
        if self.arity() != parameters.len() {
            return Err(InterpreterError::IncorrectArity {
                name: self.name.clone(),
                expected: self.arity(),
                got: parameters.len(),
            })
        }

        (self.function)(receiver, parameters)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpreterError {
    MissingMethod(String, Location),
    MissingName(String, Location),
    IntegerOverflow(Location),

    // TODO: location for these
    IncorrectArity {
        name: String,
        expected: usize,
        got: usize,
    },
    IncorrectType, // TODO more details
    InvalidAssignmentTarget(Location),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexicalContext {
    parent: Option<LexicalContextRef>,
    locals: Vec<(String, ValueRef)>,
}

impl LexicalContext {
    pub fn new_top_level() -> Self {
        LexicalContext { parent: None, locals: vec![] }
    }

    pub fn new_with_parent(parent: LexicalContextRef) -> Self {
        LexicalContext { parent: Some(parent), locals: vec![] }
    }

    pub fn rc(self) -> LexicalContextRef {
        Rc::new(RefCell::new(self))
    }

    pub fn find_local(&self, name: &str) -> Option<ValueRef> {
        self.locals.iter()
            .find_map(|(n, v)| if n == &name { Some(v.clone()) } else { None })
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow().find_local(name)))
    }

    pub fn create_local(&mut self, name: &str, value: ValueRef) {
        self.locals.push((name.into(), value))
    }
}

pub type LexicalContextRef = Rc<RefCell<LexicalContext>>;

pub struct StackFrame {
    context: InternalMethodRef,
}

pub struct Interpreter {
    types: Vec<Rc<Type>>,
    stack: Vec<StackFrame>,
}

pub type InterpreterResult = Result<ValueRef, InterpreterError>;

impl Interpreter {
    pub fn new() -> Self {
        Self {
            types: stdlib::types(),
            stack: vec![],
        }
    }

    pub fn evaluate(&mut self, node: &Node) -> InterpreterResult {
        match &node.kind {
            NodeKind::IntegerLiteral(i) => (*i).try_into()
                .map(|i| Value::new_integer(i).rc())
                .map_err(|_| InterpreterError::IntegerOverflow(node.location)),

            NodeKind::SendMessage { receiver, components } => {
                // Evaluate the receiver
                let receiver = self.evaluate(&receiver)?;
                let receiver_ref = receiver.borrow();

                let method_name = components.to_method_name();
                let method =
                    if let TypeInstance::Type(t) = &receiver_ref.type_instance {
                        t.resolve_static_method(&method_name)
                    } else {
                        receiver_ref.type_instance.get_type(&self).resolve_method(&method_name)
                    };
                if let Some(method) = method {
                    drop(receiver_ref);

                    // Evaluate parameters
                    let parameters = match components {
                        crate::parser::SendMessageComponents::Unary(_) => vec![],
                        crate::parser::SendMessageComponents::Parameterised(params) =>
                            params.iter().map(|(_, p)| self.evaluate(p)).collect::<Result<Vec<_>, _>>()?,
                    };

                    // Create a new stack frame, call the method within it, and pop the frame
                    self.stack.push(StackFrame {
                        context: method.clone(),
                    });
                    let result = (method.function)(receiver, parameters);
                    self.stack.pop();

                    result
                } else {
                    Err(InterpreterError::MissingMethod(method_name, node.location))
                }
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
                // Only supported assignment target currently is a plain identifier
                if let box Node { kind: NodeKind::Identifier(id), .. } = target {
                    let value = self.evaluate(value)?;
                    let mut context = node.context.borrow_mut();

                    if let Some(target) = context.find_local(&id) {
                        *target.borrow_mut() = value.borrow().clone();
                    } else {
                        context.create_local(&id, value.clone());
                    }

                    Ok(value)
                } else {
                    Err(InterpreterError::InvalidAssignmentTarget(target.location))
                }
            },

            NodeKind::Identifier(id) => {
                if let Some(value) = node.context.borrow().find_local(id) {
                    Ok(value)
                } else if let Some(t) = self.resolve_type(id) {
                    Ok(Value::new_type(t.clone()).rc())
                } else {
                    Err(InterpreterError::MissingName(id.into(), node.location))
                }
            }
        }
    }

    pub fn resolve_type(&self, id: &str) -> Option<Rc<Type>> {
        self.types.iter().find(|t| &t.id == id).cloned()
    }

    pub fn resolve_stdlib_type(&self, id: &str) -> Rc<Type> {
        self.resolve_type(id).expect(&format!("internal error: stdlib type {} missing", id))
    }
}

mod tests {
    use crate::{parser::Parser, tokenizer::Tokenizer, interpreter::{TypeInstance, Interpreter, InterpreterError, Value}};

    use super::ValueRef;

    fn evaluate(input: &str) -> Result<ValueRef, InterpreterError> {
        let node = Parser::parse(&Tokenizer::tokenize(input).unwrap()[..]).unwrap();
        Interpreter::new().evaluate(&node)
    }

    #[test]
    fn test_simple_interpret() {
        assert_eq!(
            evaluate("32 add: 24.").unwrap(),
            Value::new_integer(56).rc(),
        );
    }

    #[test]
    fn test_integer_overflow_on_token_conversion() {
        assert!(matches!(
            //   One more than the maximum i64
            //        vvvvvvvvvvvvvvvvvvv
            evaluate("9223372036854775808."),
            Err(InterpreterError::IntegerOverflow(..)),
        ));
    }

    #[test]
    fn test_message_precedence() {
        assert_eq!(
            evaluate("2 negate add: 7 negate.").unwrap(),
            Value::new_integer(-9).rc(),
        );

        assert_eq!(
            evaluate("(5 add: 5) sub: (3 add: 4).").unwrap(),
            Value::new_integer(3).rc(),
        );
    }

    #[test]
    fn test_sequence() {
        // A sequence evaluates to its last item
        assert_eq!(
            evaluate("1. 2. 3.").unwrap(),
            Value::new_integer(3).rc(),
        )
    }

    #[test]
    fn test_assignment() {
        assert_eq!(
            evaluate("a = 3. b = 4. a add: b.").unwrap(),
            Value::new_integer(7).rc(),
        )
    }

    #[test]
    fn test_static_methods() {
        assert_eq!(
            evaluate("Integer zero.").unwrap(),
            Value::new_integer(0).rc(),
        )
    }
}
