use crate::{source::Location, tokenizer::{Token, TokenKind, Tokenizer}};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    kind: NodeKind,
    location: Location,
}

impl Node {
    pub fn new(kind: NodeKind, location: Location) -> Self {
        Node { kind, location }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendMessageComponents {
    Unary(String),
    Parameterised(Vec<(String, Box<Node>)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    IntegerLiteral(u64),
    SendMessage {
        receiver: Box<Node>,
        components: SendMessageComponents,
    }
}

impl NodeKind {
    pub fn at(self, location: Location) -> Node {
        Node::new(self, location)
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

    fn parse_top_level(&mut self) -> Result<Node, ParserError> {
        self.parse_parameterised_send()
    }

    fn parse_parameterised_send(&mut self) -> Result<Node, ParserError> {
        let mut result = self.parse_unary_send()?;
        if let Token { kind: TokenKind::LabelIdentifier(_), location } = self.here() {
            let location = *location;

            // There are parameters are this! Parse them, and convert to a parameterised send
            let mut parameters = vec![];
            while let Token { kind: TokenKind::LabelIdentifier(id), .. } = self.here() {
                let id = id.clone();
                self.advance();
                let value = self.parse_unary_send()?;
                parameters.push((id, Box::new(value)));
            }

            result = NodeKind::SendMessage {
                receiver: Box::new(result),
                components: SendMessageComponents::Parameterised(parameters),
            }.at(location);
        }

        Ok(result)
    }

    fn parse_unary_send(&mut self) -> Result<Node, ParserError> {
        let mut result = self.parse_literal()?;
        while let Token { kind: TokenKind::Identifier(id), location } = self.here() {
            result = NodeKind::SendMessage {
                receiver: Box::new(result),
                components: SendMessageComponents::Unary(id.clone()),
            }.at(*location);
            self.advance();
        }

        Ok(result)
    }

    fn parse_literal(&mut self) -> Result<Node, ParserError> {
        if let Token { kind: TokenKind::IntegerLiteral(value), location } = self.here() {
            let node = NodeKind::IntegerLiteral(*value).at(*location);
            self.advance();
            Ok(node)
        } else {
            Err(ParserError::UnexpectedToken(self.here().clone()))
        }
    }

    pub fn parse(tokens: &'a [Token]) -> Result<Node, ParserError> {
        Self::new(tokens).parse_top_level()
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
