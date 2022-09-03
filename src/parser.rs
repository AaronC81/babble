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
    Identifier(String),
    SendMessage {
        receiver: Box<Node>,
        components: SendMessageComponents,
    },
    StatementSequence(Vec<Node>),
    Assignment {
        target: Box<Node>,
        value: Box<Node>,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    UnexpectedToken(Token),
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current_index: usize,
}

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

    fn all_tokens_consumed(&self) -> bool {
        self.current_index >= self.tokens.len()
    }

    pub fn try_or_revert<F>(&mut self, mut func: F) -> Option<Node>
    where F: FnMut(&mut Self) -> Result<Node, ParserError> {
        let previous_state = self.clone();

        match func(self) {
            Ok(node) => Some(node),
            Err(_) => {
                *self = previous_state;
                None
            }
        }
    }

    pub fn parse(tokens: &'a [Token]) -> Result<Node, ParserError> {
        let context = LexicalContext::new_top_level().rc();
        let mut parser = Self::new(tokens);
        let result = parser.parse_top_level(context)?;

        if !parser.all_tokens_consumed() {
            return Err(ParserError::UnexpectedToken(parser.here().clone()))
        }

        Ok(result)
    }

    fn parse_top_level(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        let mut seq = vec![];
        while !self.all_tokens_consumed() {
            seq.push(self.parse_single_statement(context.clone())?);
        }

        // TODO: calculate location properly
        Ok(Node {
            kind: NodeKind::StatementSequence(seq),
            location: self.tokens.first().map(|t| t.location).unwrap_or(Location::new_single(0)),
            context,
        })
    }

    fn parse_single_statement(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        let node = self.parse_assignment(context)?;

        // If there is a separator, advance past it
        if let Token { kind: TokenKind::Terminator, .. } = self.here() {
            self.advance();
        }

        Ok(node)
    }

    fn parse_assignment(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        let mut node = self.parse_parameterised_send(context.clone())?;

        // If there is an assignment operator, turn this into an assignment
        if let Token { kind: TokenKind::Assignment, .. } = self.here() {
            self.advance();
            
            // TODO: merge locations
            node.kind = NodeKind::Assignment {
                target: Box::new(node.clone()),
                value: Box::new(self.parse_single_statement(context)?),
            };
        }

        Ok(node)
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
            let node = Node {
                kind: NodeKind::IntegerLiteral(*value),
                location: *location,
                context,
            };
            self.advance();
            Ok(node)
        } else if let Token { kind: TokenKind::Identifier(id), location } = self.here() {
            let node = Node {
                kind: NodeKind::Identifier(id.clone()),
                location: *location,
                context,
            };
            self.advance();
            Ok(node)
        } else {
            Err(ParserError::UnexpectedToken(self.here().clone()))
        }
    }
}

#[test]
fn test_simple_parse() {
    let parsed = Parser::parse(&Tokenizer::tokenize("32 add: 24.").unwrap()[..]).unwrap();
    let parsed = if let Node { kind: NodeKind::StatementSequence(seq), .. } = parsed {
        assert_eq!(seq.len(), 1);
        seq[0].clone()
    } else {
        panic!("expected StatementSequence")
    };

    assert!(matches!(
        parsed,
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
