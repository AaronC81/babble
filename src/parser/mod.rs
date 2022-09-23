mod node;
pub use node::*;

mod lexical_context;
pub use lexical_context::*;

pub mod capture_analysis;

mod tests;

use crate::{tokenizer::{Token, TokenKind, TokenKeyword}, source::Location};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    UnexpectedToken(Token),
    InvalidFuncDefinitionParameter,
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current_index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let mut s = Self {
            tokens,
            current_index: 0,
        };
        s.skip_newlines(); // TODO: Temporary (maybe)
        s
    }

    fn here(&self) -> &Token {
        &self.tokens[self.current_index]
    }

    fn advance(&mut self) {
        self.current_index += 1;
        self.skip_newlines() // TODO: Temporary (maybe)
    }

    fn skip_newlines(&mut self) {
        while !self.all_tokens_consumed() && self.here().kind == TokenKind::NewLine {
            self.current_index += 1;
        }
    }

    fn all_tokens_consumed(&self) -> bool {
        self.current_index >= self.tokens.len()
        || self.tokens.get(self.current_index).map(|t| t.kind.clone()) == Some(TokenKind::EndOfFile)
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

    pub fn parse_and_analyse(tokens: &'a [Token]) -> Result<Node, ParserError> {
        let mut parsed = Self::parse(tokens)?;
        capture_analysis::populate_captures(&mut parsed);
        Ok(parsed)
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
        // Check if this is an `impl` statement, and if so, handle it differently
        if let TokenKind::Keyword(TokenKeyword::Impl) = self.here().kind {
            self.advance();

            // Parse one expression, which will evaluate to the type to implement on
            let impl_target = self.parse_expression(context.clone())?;

            // Expect an opening brace
            let TokenKind::LeftBrace = self.here().kind else {
                return Err(ParserError::UnexpectedToken(self.here().clone()))
            };
            self.advance();

            // Collect a list of function definitions within the impl, until we hit a closing brace
            let inner_context = LexicalContext::new_with_parent(context).rc();
            let mut items = vec![];
            loop {
                if let TokenKind::RightBrace = self.here().kind {
                    self.advance();
                    break
                }
                items.push(self.parse_func_definition(inner_context.clone())?);
            }

            // Construct and return node
            let loc = impl_target.location;
            return Ok(Node {
                location: loc.clone(),
                context: inner_context.clone(),
                kind: NodeKind::ImplBlock {
                    target: Box::new(impl_target),
                    body: Box::new(Node {
                        location: loc.clone(),
                        context: inner_context,
                        kind: NodeKind::StatementSequence(items),
                    }),
                },
            })
        }

        let node = self.parse_expression(context)?;

        // If there is a separator, advance past it
        if let Token { kind: TokenKind::Terminator, .. } = self.here() {
            self.advance();
        }

        Ok(node)
    }

    fn parse_expression(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        self.parse_assignment(context)
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
        if let Some(components) = self.try_parse_only_send_parameters(context.clone())? {
            let location = result.location.clone();
            result = Node {
                kind: NodeKind::SendMessage {
                    receiver: Box::new(result),
                    components,
                },
                location,
                context: LexicalContext::new_with_parent(context).rc(),
            }
        }

        Ok(result)
    }

    fn try_parse_only_send_parameters(&mut self, context: LexicalContextRef) -> Result<Option<SendMessageComponents>, ParserError> {
        if let Token { kind: TokenKind::LabelIdentifier(_), .. } = self.here() {
            // There are parameters are this! Parse them, and convert to a parameterised send
            let mut parameters = vec![];
            while let Token { kind: TokenKind::LabelIdentifier(id), .. } = self.here() {
                let id = id.clone();
                self.advance();
                let value = self.parse_unary_send(context.clone())?;
                parameters.push((id, SendMessageParameter::Parsed(Box::new(value))));
            }

            Ok(Some(SendMessageComponents::Parameterised(parameters)))
        } else {
            Ok(None)
        }
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
        } else if let Token { kind: TokenKind::StringLiteral(value), location } = self.here() {
            let node = Node {
                kind: NodeKind::StringLiteral(value.clone()),
                location: *location,
                context,
            };
            self.advance();
            Ok(node)
        } else if let Token { kind: TokenKind::Identifier(id), location } = self.here() {
            let mut node = Node {
                kind: NodeKind::Identifier(id.clone()),
                location: *location,
                context: context.clone(),
            };
            self.advance();

            // This is an enum variant constructor!
            if let Token { kind: TokenKind::Hash, .. } = self.here() {
                self.advance();
                
                // Grab the name of the variant
                let variant_name = if let Token { kind: TokenKind::Identifier(id), location } = self.here() {
                    id.clone()
                } else {
                    return Err(ParserError::UnexpectedToken(self.here().clone()))
                };
                self.advance();

                let components = self.try_parse_only_send_parameters(context)?
                    .unwrap_or(SendMessageComponents::Unary("<blank>".into()));
                
                node.kind = NodeKind::EnumVariant {
                    enum_type: Box::new(node.clone()),
                    variant_name,
                    components,
                }
            }

            Ok(node)
        } else if let Token { kind: TokenKind::LeftParen, .. } = self.here() {
            self.advance();
            let node = self.parse_expression(context)?;
            if let Token { kind: TokenKind::RightParen, .. } = self.here() {
                self.advance();
                Ok(node)
            } else {
                Err(ParserError::UnexpectedToken(self.here().clone()))
            }
        } else if let Token { kind: TokenKind::BlockStart, .. } = self.here() {
            self.advance();

            // If the first token is a pipe, then parse parameters until the next pipe
            let mut parameters = vec![];
            let mut captures = vec![];
            if let Token { kind: TokenKind::Pipe, .. } = self.here() {
                self.advance();
                loop {
                    match &self.here().kind {
                        TokenKind::Identifier(i) => {
                            parameters.push(i.clone());
                            self.advance();
                        },

                        // TODO: this capture syntax is pretty ugly since it can be interspersed
                        // into the normal parameter list, and also just kind of shouldn't be
                        // necessary at all
                        // Ideally we'll have some pass later to build this list automatically,
                        // but for now it's explicit - consider removing this syntax at some point
                        TokenKind::Star => {
                            self.advance();
                            if let TokenKind::Identifier(i) = &self.here().kind {
                                captures.push(i.clone());
                                self.advance();
                            } else {
                                return Err(ParserError::UnexpectedToken(self.here().clone()))
                            }
                        }
                        TokenKind::Pipe => {
                            self.advance();
                            break;
                        },
                        _ => return Err(ParserError::UnexpectedToken(self.here().clone()))
                    }
                }
            }

            let mut body = vec![];
            
            loop {
                if let Token { kind: TokenKind::BlockEnd, .. } = self.here() {
                    self.advance();
                    break
                }
                body.push(self.parse_single_statement(context.clone())?);
            }

            let new_context = LexicalContext::new_with_parent(context).rc();
            Ok(Node {
                // TODO
                location: body[0].location,
                kind: NodeKind::Block {
                    body: Box::new(Node {
                        location: body[0].location,
                        kind: NodeKind::StatementSequence(body),
                        context: new_context.clone(),
                    }),
                    parameters,
                    captures,
                },
                context: new_context,
            })
        } else if let Token { kind: TokenKind::Keyword(kw), location } = self.here() {
            let node = Node {
                location: *location,
                kind: match kw {
                    TokenKeyword::True => NodeKind::TrueLiteral,
                    TokenKeyword::False => NodeKind::FalseLiteral,
                    TokenKeyword::Null => NodeKind::NullLiteral,
                    TokenKeyword::Zelf => NodeKind::SelfLiteral,
                    
                    // Should have been handled earlier
                    TokenKeyword::Impl | TokenKeyword::Func =>
                        return Err(ParserError::UnexpectedToken(self.here().clone())),
                },
                context,
            };
            self.advance();
            Ok(node)
        } else {
            Err(ParserError::UnexpectedToken(self.here().clone()))
        }
    }

    fn parse_func_definition(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Definitions should always begin with `func`
        let &Token { location, kind: TokenKind::Keyword(TokenKeyword::Func) } = self.here() else {
            return self.token_error();
        };
        self.advance();

        // Parse message parameters, but ensure that the values are identifiers and transform them
        // (These will be used as the names of locals for each parameter)
        let inner_context = LexicalContext::new_with_parent(context).rc();
        let Some(mut parameters) = self.try_parse_only_send_parameters(inner_context.clone())? else {
            return self.token_error();
        };
        if let SendMessageComponents::Parameterised(ref mut components) = &mut parameters {
            for (_, internal_name) in components {
                // The "value" for this parameter should always be a plain identifier
                let SendMessageParameter::Parsed(box Node { kind: NodeKind::Identifier(id), .. }) = internal_name else {
                    return Err(ParserError::InvalidFuncDefinitionParameter);
                };

                *internal_name = SendMessageParameter::Defined(id.clone());
            }
        }

        // Expect an opening brace
        let TokenKind::LeftBrace = self.here().kind else {
            return self.token_error();
        };
        self.advance();

        // Collect a list of statements within the definition, until we hit a closing brace
        let mut items = vec![];
        loop {
            if let TokenKind::RightBrace = self.here().kind {
                self.advance();
                break
            }
            items.push(self.parse_single_statement(inner_context.clone())?);
        }

        // Construct definition
        Ok(Node {
            location,
            context: inner_context.clone(),
            kind: NodeKind::FuncDefinition {
                parameters,
                body: Box::new(Node {
                    location,
                    context: inner_context.clone(),
                    kind: NodeKind::StatementSequence(items),
                }),
            },
        })
    }
    
    fn token_error(&self) -> Result<Node, ParserError> {
        return Err(ParserError::UnexpectedToken(self.here().clone()));
    }
}
