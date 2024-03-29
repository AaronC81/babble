//! Babble's parser.
//! 
//! This is a bit of a hand-rolled mess, but it works surprisingly well. The methods are scarcely
//! documented since they are currently changing regularly due to the recursive-descent nature.

mod node;
use std::{rc::Rc, fmt::Display};

pub use node::*;

mod lexical_context;
pub use lexical_context::*;

pub mod capture_analysis;
pub mod desugar;

mod literal;
pub use literal::*;

mod pattern;
pub use pattern::*;

mod traits;
pub use traits::*;

#[cfg(test)]
mod tests;

use crate::{tokenizer::{Token, TokenKind, TokenKeyword}, source::{Location, SourceFile}};

/// An error found while parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    /// A token was unexpected at this time.
    UnexpectedToken(Token),

    /// A parameter name in a function definition was not an identifier.
    /// 
    /// This can occur because call parsing logic is re-used for definitions.
    InvalidFuncDefinitionParameter(Location),

    /// Fields used inside an enum variant are not permitted to be static - only fields within 
    /// structs are.
    StaticEnumVariantField(Location),

    /// An error occurred while parsing a pattern.
    PatternError(PatternParseError),
}

impl ParserError {
    pub fn location(&self) -> &Location {
        match self {
            ParserError::UnexpectedToken(token) => &token.location,
            ParserError::InvalidFuncDefinitionParameter(l) => l,
            ParserError::StaticEnumVariantField(l) => l,
            ParserError::PatternError(PatternParseError::InvalidNode(n)) => &n.location,
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(_) => write!(f, "unexpected token"),
            ParserError::InvalidFuncDefinitionParameter(_) => write!(f, "function definition contains an invalid parameter"),
            ParserError::StaticEnumVariantField(_) => write!(f, "enum fields cannot be static"),
            ParserError::PatternError(e) => e.fmt(f),
        }
    }
}

/// The parser state.
#[derive(Debug, Clone)]
pub struct Parser<'a> {
    source_file: Rc<SourceFile>,
    tokens: &'a [Token],
    current_index: usize,
}

impl<'a> Parser<'a> {
    /// Constructs a parser, from metadata about a source file, and a sequence of its tokens.
    pub fn new(source_file: Rc<SourceFile>, tokens: &'a [Token]) -> Self {
        let mut s = Self {
            source_file,
            tokens,
            current_index: 0,
        };
        s.skip_unused();
        s
    }

    /// Returns the token at the parser's current position.
    fn here(&self) -> &Token {
        &self.tokens[self.current_index]
    }

    /// Advances the parser's current position.
    fn advance(&mut self) {
        self.current_index += 1;
        self.skip_unused()
    }

    fn with_temporary_index<T>(&mut self, f: impl Fn(&mut Self) -> T) -> T {
        let old = self.current_index;
        let result = f(self);
        self.current_index = old;
        result
    }
    
    /// Skips over any:
    ///   - Newline tokens, as these are currently unused.
    ///   - Documentation comments, as these will be collected by backtracking if necessary.
    fn skip_unused(&mut self) {
        while !self.all_tokens_consumed() {
            match self.here().kind {
                TokenKind::NewLine | TokenKind::DocComment(_) => self.current_index += 1,
                _ => break,
            }
        }
    }

    /// Returns true if the parser has consumed every token, except the [`TokenKind::EndOfFile`]
    /// token which the stream ends with.
    fn all_tokens_consumed(&self) -> bool {
        self.current_index >= self.tokens.len()
        || self.tokens.get(self.current_index).map(|t| t.kind.clone()) == Some(TokenKind::EndOfFile)
    }

    /// Returns an iterator of the tokens which have already been consumed, most recent first.
    fn backtrack(&self) -> impl Iterator<Item = &Token> {
        self.tokens[..self.current_index].iter().rev()
    }

    pub fn parse(source_file: Rc<SourceFile>, tokens: &'a [Token]) -> Result<Node, ParserError> {
        let context = LexicalContext::new_top_level().rc();
        let mut parser = Self::new(source_file, tokens);
        let result = parser.parse_top_level(context)?;

        if !parser.all_tokens_consumed() {
            return Err(ParserError::UnexpectedToken(parser.here().clone()))
        }

        Ok(result)
    }

    pub fn parse_and_analyse(source_file: Rc<SourceFile>, tokens: &'a [Token]) -> Result<Node, ParserError> {
        let mut parsed = Self::parse(source_file, tokens)?;
        desugar::desugar_all(&mut parsed);
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
            location: self.tokens.first()
                .map(|t| t.location.clone())
                .unwrap_or_else(|| Location::new_single(self.source_file.clone(), 0)),
            context,
        })
    }

    fn parse_single_statement(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Some statements are handled differently, indicated by a keyword
        match self.here().kind {
            TokenKind::Keyword(TokenKeyword::Impl) => self.parse_impl_block(context),
            TokenKind::Keyword(TokenKeyword::Mixin) => self.parse_mixin_definition(context),
            TokenKind::Keyword(TokenKeyword::Use) => self.parse_use(context),
            TokenKind::Keyword(TokenKeyword::Func | TokenKeyword::Private | TokenKeyword::Unordered) => self.parse_func_definition(context),
            TokenKind::Keyword(TokenKeyword::Enum) => self.parse_enum_definition(context),
            TokenKind::Keyword(TokenKeyword::Struct) => self.parse_struct_definition(context),

            // Static is a bit more complicated, since it could be followed by multiple different
            // things
            TokenKind::Keyword(TokenKeyword::Static) => {
                let to_parse = self.with_temporary_index(|this| {
                    this.advance();
                    match this.here().kind {
                        TokenKind::Keyword(TokenKeyword::Func | TokenKeyword::Private | TokenKeyword::Unordered) => Some(TokenKeyword::Func),
                        TokenKind::Keyword(TokenKeyword::Use) => Some(TokenKeyword::Use),
                        _ => None
                    }
                });
                match to_parse {
                    Some(TokenKeyword::Func) => self.parse_func_definition(context),
                    Some(TokenKeyword::Use) => self.parse_use(context),
                    Some(_) => unreachable!(),
                    None => {
                        self.advance();
                        self.token_error()?
                    },
                }
            }

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
        let mut result = self.parse_binary_send(context.clone())?;
        loop {
            if let Some(components) = self.try_parse_only_send_parameters(context.clone())? {
                let location = result.location.clone();
                result = Node {
                    kind: NodeKind::SendMessage {
                        receiver: Box::new(result),
                        components,
                    },
                    location,
                    context: LexicalContext::new_with_parent(context.clone()).rc(),
                }
            }

            if self.here().kind == TokenKind::Dollar {
                self.advance();
                result = self.try_parse_append_send_unaries(result, context.clone())?;
                result = self.try_parse_append_send_binaries(result, context.clone())?;
                // ...and repeat
            } else {
                break
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
                let value = self.parse_binary_send(context.clone())?;
                parameters.push((id, SendMessageParameter::CallArgument(Box::new(value))));
            }

            Ok(Some(SendMessageComponents::Parameterised(parameters)))
        } else {
            Ok(None)
        }
    }

    fn parse_binary_send(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        let mut result = self.parse_unary_send(context.clone())?;
        result = self.try_parse_append_send_binaries(result, context)?;
        Ok(result)
    }

    fn try_parse_append_send_binaries(&mut self, mut node: Node, context: LexicalContextRef) -> Result<Node, ParserError> {
        while let Some(op) = BinaryOperation::from_token_kind(&self.here().kind) {
            let location = self.here().location.clone();
            self.advance();
            node = Node {
                kind: NodeKind::Sugar(SugarNodeKind::BinaryMessage {
                    left: Box::new(node),
                    right: Box::new(self.parse_unary_send(context.clone())?),
                    op,
                }),
                location: self.here().location.clone(),
                context: LexicalContext::new_with_parent(context.clone()).rc(),
            };
        }
        Ok(node)
    }

    fn parse_unary_send(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        let mut result = self.parse_literal(context.clone())?;
        result = self.try_parse_append_send_unaries(result, context)?;
        Ok(result)
    }

    fn try_parse_append_send_unaries(&mut self, mut node: Node, context: LexicalContextRef) -> Result<Node, ParserError> {
        while let Token { kind: TokenKind::Identifier(id), location } = self.here() {
            node = Node {
                kind: NodeKind::SendMessage {
                    receiver: Box::new(node),
                    components: SendMessageComponents::Unary(id.clone()),
                },
                location: location.clone(),
                context: context.clone(),
            };
            self.advance();
        }
        Ok(node)
    }

    fn parse_literal(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        if let Token { kind: TokenKind::IntegerLiteral { value, negative }, location } = self.here() {
            let node = Node {
                kind: NodeKind::Literal(Literal::Integer(*value as i64 * if *negative { -1 } else { 1 })),
                location: location.clone(),
                context,
            };
            self.advance();
            Ok(node)
        } else if let Token { kind: TokenKind::StringLiteral(_), .. } = self.here() {
            self.parse_string_literal(context)
        } else if let Token { kind: TokenKind::Identifier(id), location } = self.here() {
            let mut node = Node {
                kind: NodeKind::Identifier(id.clone()),
                location: location.clone(),
                context: context.clone(),
            };
            self.advance();

            // Check if this is an enum variant constructor
            if let Token { kind: TokenKind::Hash, .. } = self.here() {
                self.advance();
                
                // Parse the variant constructor
                node = self.parse_variant_constructor(node, context)?;
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
        } else if let Token {
            kind: TokenKind::BlockStart | TokenKind::QuestionMark | TokenKind::ExclamationMark,
            location
        } = self.here() {
            // What kind of block is this?
            let (uses_pattern_params, fatal) = match self.here().kind {
                TokenKind::BlockStart => (false, false),
                TokenKind::QuestionMark => (true, false),
                TokenKind::ExclamationMark => (true, true),
                _ => unreachable!(),
            };
            let location = location.clone();

            // Advance past the block start, which might be either one token or two
            self.advance();
            if uses_pattern_params {
                let TokenKind::BlockStart = self.here().kind else { self.token_error()? };
                self.advance();
            }

            // If the first token is a pipe, then parse parameters until the next pipe
            let mut patterns = vec![];
            let parameters = if uses_pattern_params {
                // Parse patterned parameters
                if let Token { kind: TokenKind::Pipe, .. } = self.here() {
                    self.advance();
                    loop {
                        // Terminators (.) are permitted to break up expressions
                        if let Token { kind: TokenKind::Terminator, .. } = self.here() {
                            self.advance();
                            continue
                        }

                        if let TokenKind::Pipe = &self.here().kind {
                            self.advance();
                            break
                        }

                        let exp = self.parse_expression(context.clone())?;
                        let pattern = Pattern::parse(exp)
                            .map_err(ParserError::PatternError)?;
                        patterns.push(pattern);
                    }
                }

                // Create one block parameter for each pattern parameter to match against
                BlockParameters::Named(
                    (0..patterns.len()).map(|i| format!("___p{i}")).collect()
                )
            } else {
                // Parse named (or any) parameters
                let parameters;
                if let Token { kind: TokenKind::Pipe, .. } = self.here() {
                    self.advance();

                    // Test for all parameter, indicated with a star
                    if let TokenKind::Star = self.here().kind {
                        // We should expect one parameter name, which will be an array
                        self.advance();
                        let name = if let Token { kind: TokenKind::Identifier(id), .. } = self.here() {
                            id.clone()
                        } else {
                            return Err(ParserError::UnexpectedToken(self.here().clone()))
                        };
                        parameters = BlockParameters::All(name);
                        self.advance();

                        // Now there should be the end of the parameter list
                        let TokenKind::Pipe = self.here().kind else { self.token_error()? };
                        self.advance();
                    } else {
                        // These are normal named parameters
                        let mut named_parameters = vec![];
                        loop {
                            match &self.here().kind {
                                TokenKind::Identifier(i) => {
                                    named_parameters.push(i.clone());
                                    self.advance();
                                },

                                TokenKind::Pipe => {
                                    self.advance();
                                    break;
                                },
                                _ => return Err(ParserError::UnexpectedToken(self.here().clone()))
                            }
                        }
                        parameters = BlockParameters::Named(named_parameters);
                    }
                } else {
                    // No parameters expected
                    parameters = BlockParameters::Named(vec![]);
                }
                parameters
            };

            let mut body = vec![];
            
            loop {
                if let Token { kind: TokenKind::BlockEnd, .. } = self.here() {
                    self.advance();
                    break
                }
                body.push(self.parse_single_statement(context.clone())?);
            }

            let new_context = LexicalContext::new_with_parent(context).rc();
            let block_node = Node {
                location: location.clone(),
                kind: NodeKind::Block {
                    body: Box::new(Node {
                        location: location.clone(),
                        kind: NodeKind::StatementSequence(body),
                        context: new_context.clone(),
                    }),
                    parameters,
                    captures: vec![],
                },
                context: new_context.clone(),
            };

            // If the block is pattern-matched, wrap in sugar
            if uses_pattern_params {
                Ok(Node {
                    location: location,
                    kind: NodeKind::Sugar(SugarNodeKind::PatternBlock {
                        block: Box::new(block_node),
                        patterns,
                        fatal,
                    }),
                    context: new_context,
                })
            } else {
                Ok(block_node)
            }
        } else if let Token { kind: TokenKind::Keyword(TokenKeyword::Return), location } = self.here() {
            let location = location.clone();
            self.advance();

            // Parse the value to return
            let value = self.parse_expression(context.clone())?;

            Ok(Node {
                kind: NodeKind::Sugar(SugarNodeKind::Return(Box::new(value))),
                context,
                location,
            })
        } else if let Token { kind: TokenKind::Hash, location } = self.here() {
            let location = location.clone();
            self.advance();

            // First, check if this is supposed to be a shorthand enum constructor
            if let Token { kind: TokenKind::Identifier(id), .. } = self.here() {
                // Yep! Parse the enum constructor
                let node = self.parse_variant_constructor(Node {
                    kind: NodeKind::Sugar(SugarNodeKind::ShorthandVariantConstructor),
                    location,
                    context: context.clone(),
                }, context)?;
                return Ok(node);
            }
            
            // This is a collection literal
            let Token { kind: TokenKind::LeftBrace, .. } = self.here() else {
                return Err(ParserError::UnexpectedToken(self.here().clone()))
            };
            self.advance();

            // Parse expressions until the closing brace
            let mut items = vec![];
            loop {
                // Terminators (.) are permitted to break up expressions
                if let Token { kind: TokenKind::Terminator, .. } = self.here() {
                    self.advance();
                    continue
                }

                if let Token { kind: TokenKind::RightBrace, .. } = self.here() {
                    self.advance();
                    break;
                }

                items.push(self.parse_expression(context.clone())?);
            }

            Ok(Node {
                kind: NodeKind::Array(items),
                location,
                context,
            })
        } else if let Token { kind: TokenKind::Ampersand, location } = self.here() {
            let location = location.clone();
            self.advance();
            let Token { kind: TokenKind::Identifier(i), .. } = self.here() else {
                self.token_error()?
            };
            let method_name = i.clone();
            self.advance();

            Ok(Node {
                kind: NodeKind::Sugar(SugarNodeKind::ShorthandBlock(method_name)),
                location,
                context,
            })
        } else if let Token { kind: TokenKind::Keyword(kw), location } = self.here() {
            let node = Node {
                location: location.clone(),
                kind: match kw {
                    TokenKeyword::True => NodeKind::Literal(Literal::True),
                    TokenKeyword::False => NodeKind::Literal(Literal::False),
                    TokenKeyword::Null => NodeKind::Literal(Literal::Null),
                    TokenKeyword::Zelf => NodeKind::SelfAccess,
                    
                    // Should have been handled earlier
                    TokenKeyword::Impl
                    | TokenKeyword::Func
                    | TokenKeyword::Struct
                    | TokenKeyword::Enum 
                    | TokenKeyword::Static
                    | TokenKeyword::Mixin
                    | TokenKeyword::Use
                    | TokenKeyword::Return
                    | TokenKeyword::Unordered
                    | TokenKeyword::Private =>
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

    fn parse_string_literal(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // A string literal must start with... y'know, a string literal
        let Token { kind: TokenKind::StringLiteral(value), location } = self.here() else {
            self.token_error()?
        };

        // Construct string literal node
        let node = Node {
            kind: NodeKind::Literal(Literal::String(value.clone())),
            location: location.clone(),
            context: context.clone(),
        };
        self.advance();

        // Are there interpolations following this string?
        if let Token { kind: TokenKind::StringInterpolationStart, .. } = self.here() {
            // Yep! Parse interpolations
            let location = node.location.clone();
            let context = node.context.clone();
            let mut parts = vec![node];

            while let Token { kind: TokenKind::StringInterpolationStart, .. } = self.here() {
                // Parse and insert interpolated expression
                self.advance();
                let expr = self.parse_expression(context.clone())?;
                
                let Token { kind: TokenKind::StringInterpolationEnd, .. } = self.here() else {
                    println!("missing interp end, got {:?}", self.here().kind);
                    self.token_error()?
                };
                self.advance();
                parts.push(expr);

                // After an interpolation, we expect to find another string literal
                let Token { kind: TokenKind::StringLiteral(string), .. } = self.here() else {
                    println!("missing chain string");
                    self.token_error()?
                };
                parts.push(Node {
                    kind: NodeKind::Literal(Literal::String(string.clone())),
                    location: location.clone(),
                    context: context.clone(),
                });
                self.advance();
            }

            Ok(Node {
                kind: NodeKind::Sugar(SugarNodeKind::StringInterpolation(parts)),
                location,
                context,
            })
        } else {
            // Nope! Just return the string literal
            Ok(node)
        }
    }

    fn parse_impl_block(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Definitions should always begin with `impl`
        let &Token { kind: TokenKind::Keyword(TokenKeyword::Impl), ref location } = self.here() else {
            self.token_error()?;
        };
        let location = location.clone();
        self.advance();

        // Parse one expression, which will evaluate to the type to implement on, and the body
        let impl_target = self.parse_expression(context.clone())?;
        let body = self.parse_type_definition_body(context)?;

        // Construct and return node
        Ok(Node {
            location,
            context: body.context.clone(),
            kind: NodeKind::ImplBlock {
                target: Box::new(impl_target),
                body: Box::new(body),
            },
        })
    }

    fn parse_type_definition_body(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Expect an opening brace
        let &Token { kind: TokenKind::LeftBrace, ref location } = self.here() else {
            return Err(ParserError::UnexpectedToken(self.here().clone()))
        };
        let location = location.clone();
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

        Ok(Node {
            location,
            context: inner_context,
            kind: NodeKind::StatementSequence(items),
        })
    }

    fn parse_func_definition(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // The definition can optionally begin with `static` or `private` keywords, in any order
        let location;
        let mut visibility = MethodVisibility::default();
        let mut is_static = false;
        let mut is_unordered = false;
        loop {
            if let &Token { kind: TokenKind::Keyword(TokenKeyword::Static), .. } = self.here() {
                self.advance();
                is_static = true;
            } else if let &Token { kind: TokenKind::Keyword(TokenKeyword::Private), .. } = self.here() {
                self.advance();
                visibility = MethodVisibility::Private;
            } else if let &Token { kind: TokenKind::Keyword(TokenKeyword::Unordered), .. } = self.here() {
                self.advance();
                is_unordered = true;
            } else if let &Token { location: ref loc, kind: TokenKind::Keyword(TokenKeyword::Func) } = self.here() {
                location = loc.clone();
                self.advance();
                break;
            } else {
                self.token_error()?;
            }
        }

        // At this point, we can be pretty confident that we've found a function definition. As a
        // result, iterate backwards through tokens to collect documentation comments.
        let mut doc_comments = vec![];
        for token in self.backtrack() {
            match &token.kind {
                // Guaranteed to appear, or inconsequential, so just ignore them
                TokenKind::Keyword(TokenKeyword::Static | TokenKeyword::Func) => (),
                TokenKind::NewLine => (),

                // Gather doc comments - remember we're encountering them in reverse order
                TokenKind::DocComment(comment) => doc_comments.insert(0, comment.trim()),

                // Anything else represents a break in our comments
                _ => break,
            }
        }
        let documentation = if doc_comments.is_empty() {
            None
        } else {
            Some(doc_comments.join("\n"))
        };

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
                let SendMessageParameter::CallArgument(box Node { kind: NodeKind::Identifier(id), .. }) = internal_name else {
                    return Err(ParserError::InvalidFuncDefinitionParameter(location.clone()));
                };

                *internal_name = SendMessageParameter::DefinitionParameter(id.clone());
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
            location: location.clone(),
            context: inner_context.clone(),
            kind: NodeKind::FuncDefinition {
                parameters,
                body: Box::new(Node {
                    location,
                    context: inner_context,
                    kind: NodeKind::StatementSequence(items),
                }),
                is_static,
                is_unordered,
                documentation,
                visibility,
            },
        })
    }

    fn parse_enum_definition(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Definitions should always begin with `enum` - parse docs while we're here too
        let &Token { ref location, kind: TokenKind::Keyword(TokenKeyword::Enum) } = self.here() else {
            self.token_error()?;
        };
        let documentation = self.parse_doc_comments();
        let location = location.clone();
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
            let location = self.here().location.clone();
            let (name, instance_fields, static_fields) = self.parse_data_layout()?;

            if !static_fields.is_empty() {
                return Err(ParserError::StaticEnumVariantField(location));
            }

            variants.push(Variant { name, fields: instance_fields });
        }
        
        Ok(Node {
            location,
            context: inner_context,
            kind: NodeKind::EnumDefinition { name, variants, documentation },
        })
    }

    fn parse_struct_definition(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Definitions should always begin with `struct` - parse docs while we're here too 
        let &Token { ref location, kind: TokenKind::Keyword(TokenKeyword::Struct) } = self.here() else {
            self.token_error()?;
        };
        let documentation = self.parse_doc_comments();
        let location = location.clone();
        self.advance();
        
        // Parse its contents
        let (name, instance_fields, static_fields) = self.parse_data_layout()?;
        
        Ok(Node {
            location,
            context,
            kind: NodeKind::StructDefinition {
                name,
                instance_fields,
                static_fields,
                documentation,
            }
        })
    }

    fn parse_mixin_definition(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Definitions should always begin with `mixin` - parse docs while we're here too
        let &Token { kind: TokenKind::Keyword(TokenKeyword::Mixin), ref location } = self.here() else {
            self.token_error()?;
        };
        let documentation = self.parse_doc_comments();
        let location = location.clone();
        self.advance();

        // Get an identifier for the name of the mixin
        let TokenKind::Identifier(ref name) = self.here().kind else {
            self.token_error()?;
        };
        let name = name.clone();
        self.advance();

        // Expect a terminator
        let TokenKind::Terminator = self.here().kind else {
            return Err(ParserError::UnexpectedToken(self.here().clone()))
        };
        self.advance();

        // Construct and return node
        Ok(Node {
            location,
            context,
            kind: NodeKind::MixinDefinition { name, documentation },
        })
    }

    fn parse_data_layout(&mut self) -> Result<(String, Vec<String>, Vec<String>), ParserError> {
        // Parse name
        let TokenKind::Identifier(name) = &self.here().kind else {
            self.token_error()?;
        };
        let name = name.clone();
        self.advance();

        // Parse fields, each as a name
        let mut instance_fields = vec![];
        let mut static_fields = vec![];
        let mut next_field_is_static = false;
        loop {
            match self.here().kind {
                TokenKind::Keyword(TokenKeyword::Static) => {
                    if next_field_is_static {
                        self.token_error()?;
                    }
                    next_field_is_static = true;
                    self.advance();
                },
                TokenKind::Identifier(ref name) => {
                    if next_field_is_static {
                        static_fields.push(name.clone());
                    } else {
                        instance_fields.push(name.clone());
                    }
                    next_field_is_static = false;
                    self.advance();
                },
                TokenKind::Terminator => {
                    self.advance();
                    break
                },
                _ => self.token_error()?,
            }
        }

        Ok((name, instance_fields, static_fields))
    }

    fn parse_doc_comments(&self) -> Option<String> {
        let mut doc_comments = vec![];

        for token in self.backtrack() {
            let TokenKind::DocComment(ref comment) = token.kind else {
                break;
            };

            // Gather doc comments - remember we're encountering them in reverse order
            doc_comments.insert(0, comment.trim());
        }
        
        if doc_comments.is_empty() {
            None
        } else {
            Some(doc_comments.join("\n"))
        }
    }

    fn parse_use(&mut self, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Definitions are permitted to begin with `static`
        let is_static = if let TokenKind::Keyword(TokenKeyword::Static) = self.here().kind {
            self.advance();
            true
        } else {
            false
        };

        // After this, `use` must be next
        let &Token { kind: TokenKind::Keyword(TokenKeyword::Use), ref location } = self.here() else {
            self.token_error()?;
        };
        let location = location.clone();
        self.advance();

        // Parse one expression, which should evaluate to the mixin to use
        let mixin = self.parse_expression(context.clone())?;

        // Must end with a terminator
        let TokenKind::Terminator = self.here().kind else {
            self.token_error()?;
        };
        self.advance();

        // Construct and return node
        Ok(Node {
            location,
            context,
            kind: NodeKind::Use {
                mixin: Box::new(mixin),
                is_static,
            },
        })        
    }

    fn parse_variant_constructor(&mut self, enum_type: Node, context: LexicalContextRef) -> Result<Node, ParserError> {
        // Grab the name of the variant
        let variant_name = if let Token { kind: TokenKind::Identifier(id), .. } = self.here() {
            id.clone()
        } else {
            return Err(ParserError::UnexpectedToken(self.here().clone()))
        };
        self.advance();

        // Parse out its components
        let components = self.try_parse_only_send_parameters(context.clone())?
            .unwrap_or_else(|| SendMessageComponents::Blank);
        
        Ok(Node {
            location: enum_type.location.clone(),
            context,
            kind: NodeKind::EnumVariant {
                enum_type: Box::new(enum_type),
                variant_name,
                components,
            }
        })
    }
    
    fn token_error(&self) -> Result<!, ParserError> {
        return Err(ParserError::UnexpectedToken(self.here().clone()));
    }
}
