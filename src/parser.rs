use crate::{source::Location, tokenizer::{Token, TokenKind, Tokenizer}, interpreter::{LexicalContext, LexicalContextRef}};

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
    Parameterised(Vec<(String, Box<Node>)>),
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    IntegerLiteral(u64),
    SendMessage {
        receiver: Box<Node>,
        components: SendMessageComponents,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    UnexpectedToken(Token),
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    current_index: usize,
}

// TODO: shift . if found after a message
// TODO: complain if leftover tokens
impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current_index: 0,
        }
    }

    fn here(&self) -> &Token {
        &self.tokens[self.current_index]
    }

    fn advance(&mut self) {
        self.current_index += 1;
    }

    fn parse_top_level(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        self.parse_parameterised_send(context)
    }

    fn parse_parameterised_send(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        let mut result = self.parse_unary_send(context.clone())?;
        if let Token { kind: TokenKind::LabelIdentifier(_), location } = self.here() {
            let location = *location;

            // There are parameters are this! Parse them, and convert to a parameterised send
            let mut parameters = vec![];
            while let Token { kind: TokenKind::LabelIdentifier(id), .. } = self.here() {
                let id = id.clone();
                self.advance();
                let value = self.parse_unary_send(context.clone())?;
                parameters.push((id, Box::new(value)));
            }

            result = Node {
                kind: NodeKind::SendMessage {
                    receiver: Box::new(result),
                    components: SendMessageComponents::Parameterised(parameters),
                },
                location,
                context: LexicalContext::new_with_parent(context).rc(),
            }
        }

        Ok(result)
    }

    fn parse_unary_send(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        let mut result = self.parse_literal(context.clone())?;
        while let Token { kind: TokenKind::Identifier(id), location } = self.here() {
            result = Node {
                kind: NodeKind::SendMessage {
                    receiver: Box::new(result),
                    components: SendMessageComponents::Unary(id.clone()),
                },
                location: *location,
                context: context.clone(),
            };
            self.advance();
        }

        Ok(result)
    }

    fn parse_literal(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        if let Token { kind: TokenKind::IntegerLiteral(value), location } = self.here() {
            Ok(Node {
                kind: NodeKind::IntegerLiteral(*value),
                location: *location,
                context: context,
            })
        } else {
            Err(ParserError::UnexpectedToken(self.here().clone()))
        }
    }

    pub fn parse(tokens: &'a [Token]) -> Result<Node, ParserError> {
        Self::new(tokens).parse_top_level(LexicalContext::new_top_level().rc())
    }
}

#[test]
fn test_simple_parse() {
    assert!(matches!(
        Parser::parse(&Tokenizer::tokenize("32 add: 24.").unwrap()[..]).unwrap(),
        Node {
            kind: NodeKind::SendMessage {
                receiver: box Node { kind: NodeKind::IntegerLiteral(32), .. },
                components: SendMessageComponents::Parameterised(params)
            },
            ..
        } if matches!(&params[..], [
            (p1, box Node { kind: NodeKind::IntegerLiteral(24), .. }),
        ] if p1 == "add")
    ))
}
