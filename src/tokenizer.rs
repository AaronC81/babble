use crate::source::Location;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
}

impl Token {
    pub fn new(kind: TokenKind, location: Location) -> Self {
        Token { kind, location }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Terminator,
    NewLine,
    Assignment,
    Pipe,
    Star,
    Hash,
    Keyword(TokenKeyword),

    BlockStart,
    BlockEnd,

    LeftParen,
    RightParen,

    LeftBrace,
    RightBrace,

    Identifier(String),
    LabelIdentifier(String),

    IntegerLiteral(u64),
    StringLiteral(String),

    EndOfFile,
}

impl TokenKind {
    pub fn at(self, location: Location) -> Token {
        Token::new(self, location)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKeyword {
    True,
    False,
    Null,
    Impl,
    Func,
    Zelf, // Self
    Enum,
    Struct,
    Static,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenizerError {
    UnexpectedCharacter(char, Location),
    IntegerLiteralOverflow(Location),
}

#[derive(Debug, Clone)]
pub enum TokenizerState {
    Idle,
    CollectingWhitespaceSeparated(Token),
    Comment,
}

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
    chars: &'a [char],
    current_index: usize,
    state: TokenizerState,
    tokens: Vec<Token>
}

impl<'a> Tokenizer<'a> {
    fn new(chars: &'a [char]) -> Self {
        Self {
            chars,
            current_index: 0,
            state: TokenizerState::Idle,
            tokens: vec![],
        }
    }

    fn here(&self) -> char {
        self.chars[self.current_index]
    }

    fn here_loc(&self) -> Location {
        Location::new_single(self.current_index)
    }

    fn peek(&self) -> char {
        *self.chars.get(self.current_index + 1).unwrap_or(&' ')
    }

    fn advance(&mut self) {
        self.current_index += 1;
    }

    fn step(&mut self) -> Result<(), TokenizerError> {
        let here = self.here();

        match &mut self.state {
            TokenizerState::Idle => {
                match here {
                    // Newlines are significant...
                    '\n' => self.tokens.push(TokenKind::NewLine.at(self.here_loc())),

                    // ...but other whitespace is not
                    _ if here.is_whitespace() => (),

                    // Start of an identifier
                    _ if here.is_alphabetic() || here == '_' => {
                        self.state = TokenizerState::CollectingWhitespaceSeparated(
                            TokenKind::Identifier(here.to_string()).at(self.here_loc()),
                        )
                    }

                    // Start of an integer literal
                    _ if here.is_numeric() => {
                        self.state = TokenizerState::CollectingWhitespaceSeparated(
                            TokenKind::IntegerLiteral(here.to_string().parse().unwrap()).at(self.here_loc()),
                        )
                    }

                    // Start of a string literal
                    '"' => {
                        self.state = TokenizerState::CollectingWhitespaceSeparated(
                            TokenKind::StringLiteral("".into()).at(self.here_loc()),
                        )
                    }

                    // Simple symbols
                    '=' => self.tokens.push(TokenKind::Assignment.at(self.here_loc())),
                    '.' => self.tokens.push(TokenKind::Terminator.at(self.here_loc())),
                    '[' => self.tokens.push(TokenKind::BlockStart.at(self.here_loc())),
                    ']' => self.tokens.push(TokenKind::BlockEnd.at(self.here_loc())),
                    '(' => self.tokens.push(TokenKind::LeftParen.at(self.here_loc())),
                    ')' => self.tokens.push(TokenKind::RightParen.at(self.here_loc())),
                    '{' => self.tokens.push(TokenKind::LeftBrace.at(self.here_loc())),
                    '}' => self.tokens.push(TokenKind::RightBrace.at(self.here_loc())),
                    '|' => self.tokens.push(TokenKind::Pipe.at(self.here_loc())),
                    '*' => self.tokens.push(TokenKind::Star.at(self.here_loc())),
                    '#' => self.tokens.push(TokenKind::Hash.at(self.here_loc())),

                    // Start of a comment
                    '/' if self.peek() == '/' => {
                        self.advance();
                        self.state = TokenizerState::Comment;
                    }

                    _ => return Err(TokenizerError::UnexpectedCharacter(here, self.here_loc())),
                }
            },

            TokenizerState::CollectingWhitespaceSeparated(ref mut token) => {
                match token.kind {
                    TokenKind::Identifier(ref mut id) => {
                        match here {
                            // Extend the token being parsed
                            _ if here.is_alphanumeric() || here == '_' => {
                                id.push(here);
                                token.location.length += 1;
                            },

                            // If we encounter a `:`, turn this into a label identifier and finish
                            // parsing it
                            ':' => {
                                // We don't include the `:` in our result string, but we will extend
                                // the location length by one to match it from the source
                                token.kind = TokenKind::LabelIdentifier(id.clone());
                                token.location.length += 1;

                                self.tokens.push(token.clone());
                                self.state = TokenizerState::Idle;
                            },
                            
                            // If we encounter another character, finish parsing this identifier.
                            // Explicitly return to avoid advancing the pointer.
                            _ => {
                                // But first, check if the token happens to be a keyword
                                if let Some(keyword) = Self::to_keyword(id) {
                                    token.kind = TokenKind::Keyword(keyword);
                                }

                                self.tokens.push(token.clone());
                                self.state = TokenizerState::Idle;

                                return Ok(())
                            },
                        }
                    }

                    TokenKind::IntegerLiteral(ref mut value) => {
                        match here {
                            // Extend the token being parsed
                            _ if here.is_numeric() => {
                                let maybe_value = value
                                    .checked_mul(10)
                                    .and_then(|x| x.checked_add(here.to_string().parse().unwrap()));

                                *value = maybe_value.ok_or(
                                    TokenizerError::IntegerLiteralOverflow(token.location)
                                )?;

                                token.location.length += 1;
                            },
                            
                            // If we encounter another character, finish parsing this integer.
                            // Explicitly return to avoid advancing the pointer.
                            _ => {
                                self.tokens.push(token.clone());
                                self.state = TokenizerState::Idle;

                                return Ok(())
                            },
                        }
                    }

                    TokenKind::StringLiteral(ref mut value) => {
                        // TODO: escapes
                        match here {
                            // Finish on matching quote
                            '"' => {
                                self.tokens.push(token.clone());
                                self.state = TokenizerState::Idle;
                            },

                            _ => {
                                value.push(here)
                            }
                        }
                    }

                    // Other tokens are one-shot, and won't ever be stored in the state
                    _ => unreachable!(),
                }
            }
        
            TokenizerState::Comment => {
                if here == '\n' {
                    self.tokens.push(TokenKind::NewLine.at(self.here_loc()));
                    self.state = TokenizerState::Idle
                }
            }
        }

        self.advance();
        Ok(())
    }

    pub fn to_keyword(keyword: &str) -> Option<TokenKeyword> {
        match keyword {
            "true"    => Some(TokenKeyword::True),
            "false"   => Some(TokenKeyword::False),
            "null"    => Some(TokenKeyword::Null),
            "impl"    => Some(TokenKeyword::Impl),
            "func"    => Some(TokenKeyword::Func),
            "self"    => Some(TokenKeyword::Zelf),
            "enum"    => Some(TokenKeyword::Enum),
            "struct"  => Some(TokenKeyword::Struct),
            "static"  => Some(TokenKeyword::Static),
            _         => None,
        }
    }

    pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenizerError> {
        let chars = input.chars().collect::<Vec<_>>();
        let mut tokenizer = Tokenizer::new(&chars[..]);

        while tokenizer.current_index < chars.len() {
            tokenizer.step()?;
        }

        // If the tokenizer was parsing an arbitrary-length token, shift it now
        if let TokenizerState::CollectingWhitespaceSeparated(ref mut t) = tokenizer.state {
            // Keyword check
            if let TokenKind::Identifier(id) = &t.kind {
                if let Some(keyword) = Self::to_keyword(&id) {
                    t.kind = TokenKind::Keyword(keyword);
                }
            }
            
            tokenizer.tokens.push(t.clone());
        }

        tokenizer.tokens.push(TokenKind::EndOfFile.at(tokenizer.here_loc()));

        Ok(tokenizer.tokens)
    }
}

#[test]
fn test_simple_tokenize() {
    assert_eq!(
        Tokenizer::tokenize("32 add: 24."),
        Ok(vec![
            Token::new(TokenKind::IntegerLiteral(32), Location::new(0, 2)),
            Token::new(TokenKind::LabelIdentifier("add".into()), Location::new(3, 4)),
            Token::new(TokenKind::IntegerLiteral(24), Location::new(8, 2)),
            Token::new(TokenKind::Terminator, Location::new_single(10)),
            Token::new(TokenKind::EndOfFile, Location::new_single(11)),
        ])
    )
}
