mod node;
pub use node::*;

mod lexical_context;
pub use lexical_context::*;

pub mod capture_analysis;

#[cfg(test)]
mod tests;

use crate::{tokenizer::{Token, TokenKind, TokenKeyword}, source::Location, interpreter::Variant};

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
            location: self.tokens.first().map(|t| t.location).unwrap_or_else(|| Location::new_single(0)),
            context,
        })
    }

    fn parse_single_statement(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Some statements are handled differently, indicated by a keyword
        match self.here().kind {
            TokenKind::Keyword(TokenKeyword::Impl) => self.parse_impl_block(context),
            TokenKind::Keyword(TokenKeyword::Func | TokenKeyword::Static) => self.parse_func_definition(context),
            TokenKind::Keyword(TokenKeyword::Enum) => self.parse_enum_definition(context),
            TokenKind::Keyword(TokenKeyword::Struct) => self.parse_struct_definition(context),

            // Just a normal node!
            _ => {
                let node = self.parse_expression(context)?;

                // If there is a separator, advance past it
                if let Token { kind: TokenKind::Terminator, .. } = self.here() {
                    self.advance();
                }
        
                Ok(node)    
            }
        }
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
            let location = result.location;
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
                let variant_name = if let Token { kind: TokenKind::Identifier(id), .. } = self.here() {
                    id.clone()
                } else {
                    return Err(ParserError::UnexpectedToken(self.here().clone()))
                };
                self.advance();

                let components = self.try_parse_only_send_parameters(context)?
                    .unwrap_or_else(|| SendMessageComponents::Unary("<blank>".into()));
                
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
                    TokenKeyword::Impl
                    | TokenKeyword::Func
                    | TokenKeyword::Struct
                    | TokenKeyword::Enum 
                    | TokenKeyword::Static =>
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

    fn parse_impl_block(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Definitions should always begin with `impl`
        let TokenKind::Keyword(TokenKeyword::Impl) = self.here().kind else {
            self.token_error()?;
        };
        self.advance();

        // Parse one expression, which will evaluate to the type to implement on
        let impl_target = self.parse_expression(context.clone())?;

        // Expect an opening brace
        let TokenKind::LeftBrace = self.here().kind else {
            return Err(ParserError::UnexpectedToken(self.here().clone()))
        };
        self.advance();

        // Collect a list of items within the impl, until we hit a closing brace
        let inner_context = LexicalContext::new_with_parent(context).rc();
        let mut items = vec![];
        loop {
            if let TokenKind::RightBrace = self.here().kind {
                self.advance();
                break
            }
            items.push(self.parse_single_statement(inner_context.clone())?);
        }

        // Construct and return node
        let location = impl_target.location;
        Ok(Node {
            location,
            context: inner_context.clone(),
            kind: NodeKind::ImplBlock {
                target: Box::new(impl_target),
                body: Box::new(Node {
                    location,
                    context: inner_context,
                    kind: NodeKind::StatementSequence(items),
                }),
            },
        })
    }

    fn parse_func_definition(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // The definition can optionally begin with a `static` keyword
        let is_static =
            if let &Token { kind: TokenKind::Keyword(TokenKeyword::Static), .. } = self.here() {
                self.advance();
                true
            } else {
                false
            };

        // After this, definitions should always begin with `func`
        let &Token { location, kind: TokenKind::Keyword(TokenKeyword::Func) } = self.here() else {
            self.token_error()?;
        };
        self.advance();

        // Parse message parameters, but ensure that the values are identifiers and transform them
        // (These will be used as the names of locals for each parameter)
        let inner_context = LexicalContext::new_with_parent(context).rc();
        let mut parameters = 
            // `try_parse_only_send_parameters` doesn't catch unary parameters, so do this ourselves
            if let TokenKind::Identifier(id) = &self.here().kind {
                let id = id.clone();
                self.advance();
                SendMessageComponents::Unary(id)
            } else {
                let Some(parameters) = self.try_parse_only_send_parameters(inner_context.clone())? else {
                    self.token_error()?;
                };
                parameters
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
            self.token_error()?;
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
                    context: inner_context,
                    kind: NodeKind::StatementSequence(items),
                }),
                is_static,
            },
        })
    }

    fn parse_enum_definition(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Definitions should always begin with `enum`
        let &Token { location, kind: TokenKind::Keyword(TokenKeyword::Enum) } = self.here() else {
            self.token_error()?;
        };
        self.advance();

        // Parse the name and opening brace
        let TokenKind::Identifier(name) = &self.here().kind else {
            self.token_error()?;
        };
        let name = name.clone();
        self.advance();
        let TokenKind::LeftBrace = self.here().kind else {
            self.token_error()?;
        };
        self.advance();

        // Following this is a list of variants, until the closing brace
        let inner_context = LexicalContext::new_with_parent(context).rc();
        let mut variants = vec![];
        loop {
            if let TokenKind::RightBrace = self.here().kind {
                self.advance();
                break
            }
            let (name, fields) = self.parse_data_layout()?;
            variants.push(Variant { name, fields });
        }
        
        Ok(Node {
            location,
            context: inner_context,
            kind: NodeKind::EnumDefinition { name, variants },
        })
    }

    fn parse_struct_definition(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Definitions should always begin with `struct`
        let &Token { location, kind: TokenKind::Keyword(TokenKeyword::Struct) } = self.here() else {
            self.token_error()?;
        };
        self.advance();
        
        // Parse its contents
        let (name, fields) = self.parse_data_layout()?;
        
        Ok(Node {
            location,
            context,
            kind: NodeKind::StructDefinition {
                name,
                fields,
            }
        })
    }

    fn parse_data_layout(&mut self) -> Result<(String, Vec<String>), ParserError> {
        // Parse name
        let TokenKind::Identifier(name) = &self.here().kind else {
            self.token_error()?;
        };
        let name = name.clone();
        self.advance();

        // Parse fields, each as a name
        let mut fields = vec![];
        loop {
            if let TokenKind::Terminator = self.here().kind {
                self.advance();
                break
            }

            if let TokenKind::Identifier(name) = &self.here().kind {
                fields.push(name.clone());
                self.advance();
            } else {
                self.token_error()?;
            }
        }

        Ok((name, fields))
    }
    
    fn token_error(&self) -> Result<!, ParserError> {
        return Err(ParserError::UnexpectedToken(self.here().clone()));
    }
}
