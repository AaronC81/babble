//! Babble's tokenizer, which is a prerequisite step to parsing.
//! 
//! The tokenizer removes all whitespace except newlines, which are preserved as special tokens,
//! although the parser currently skips over them.

use std::rc::Rc;

use crate::source::{Location, SourceFile};

/// A token from the source file.
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

/// The kind of a token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Terminator,
    NewLine,
    Assignment,
    Pipe,
    Star,
    Hash,
    Keyword(TokenKeyword),
    QuestionMark,
    ExclamationMark,
    Dollar,
    Ampersand,

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

    DocComment(String),

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
    Mixin,
    Use,
    Return,
}

/// An error encountered while tokenizing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenizerError {
    /// A character did not correspond to any valid token.
    UnexpectedCharacter(char, Location),

    /// While tokenizing, an integer literal would have overflown.
    IntegerLiteralOverflow(Location),

    /// A string literal used an invalid escape sequence.
    InvalidEscapeSequence(Location),
}

/// The current state of the tokenizer.
#[derive(Debug, Clone)]
pub enum TokenizerState {
    /// No token is currently being collected.
    Idle,

    /// A token is being collected which will end when whitespace is encountered.
    CollectingWhitespaceSeparated { token: Token, next_is_escape_sequence: bool },

    /// A comment is being collected, which will end on a newline.
    Comment,

    /// A documentation comment is being collected, which will end on a newline.
    DocComment(Token),
}

/// The current tokenizer state.
#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
    source_file: Rc<SourceFile>,
    chars: &'a [char],
    current_index: usize,
    state: TokenizerState,
    tokens: Vec<Token>
}

impl<'a> Tokenizer<'a> {
    fn new(source_file: Rc<SourceFile>, chars: &'a [char]) -> Self {
        Self {
            source_file,
            chars,
            current_index: 0,
            state: TokenizerState::Idle,
            tokens: vec![],
        }
    }

    /// The character at the current position.
    fn here(&self) -> char {
        self.chars[self.current_index]
    }

    /// A single-character [`Location`] referencing the current position. 
    fn here_loc(&self) -> Location {
        Location::new_single(self.source_file.clone(), self.current_index)
    }

    /// The character at the next position, or whitespace if at the end.
    fn peek(&self) -> char {
        *self.chars.get(self.current_index + 1).unwrap_or(&' ')
    }

    /// Advances the current character.
    fn advance(&mut self) {
        self.current_index += 1;
    }

    /// Performs one step of the tokenizer.
    fn step(&mut self) -> Result<(), TokenizerError> {
        let here = self.here();
        let here_loc = self.here_loc();

        match &mut self.state {
            TokenizerState::Idle => {
                match here {
                    // Newlines are significant...
                    '\n' => self.tokens.push(TokenKind::NewLine.at(self.here_loc())),

                    // ...but other whitespace is not
                    _ if here.is_whitespace() => (),

                    // Start of an identifier
                    _ if here.is_alphabetic() || here == '_' => {
                        self.state = TokenizerState::CollectingWhitespaceSeparated {
                            token: TokenKind::Identifier(here.to_string()).at(self.here_loc()),
                            next_is_escape_sequence: false,
                        }
                    }

                    // Start of an integer literal
                    _ if here.is_numeric() => {
                        self.state = TokenizerState::CollectingWhitespaceSeparated {
                            token: TokenKind::IntegerLiteral(here.to_string().parse().unwrap()).at(self.here_loc()),
                            next_is_escape_sequence: false,
                        }
                    }

                    // Start of a string literal
                    '"' => {
                        self.state = TokenizerState::CollectingWhitespaceSeparated {
                            token: TokenKind::StringLiteral("".into()).at(self.here_loc()),
                            next_is_escape_sequence: false,
                        }
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
                    '?' => self.tokens.push(TokenKind::QuestionMark.at(self.here_loc())),
                    '!' => self.tokens.push(TokenKind::ExclamationMark.at(self.here_loc())),
                    '$' => self.tokens.push(TokenKind::Dollar.at(self.here_loc())),
                    '&' => self.tokens.push(TokenKind::Ampersand.at(self.here_loc())),

                    // Start of a comment
                    '/' if self.peek() == '/' => {
                        self.advance();
                        // ...or a doc-comment!
                        if self.peek() == '/' {
                            self.advance();
                            self.state = TokenizerState::DocComment(Token {
                                kind: TokenKind::DocComment("".into()),
                                location: self.here_loc(),
                            });
                        } else {
                            self.state = TokenizerState::Comment;
                        }
                    }

                    _ => return Err(TokenizerError::UnexpectedCharacter(here, self.here_loc())),
                }
            },

            TokenizerState::CollectingWhitespaceSeparated { ref mut token, ref mut next_is_escape_sequence }  => {
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
                                    TokenizerError::IntegerLiteralOverflow(token.location.clone())
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
                        match here {
                            // If the last character started an escape sequence, deal with that
                            c if *next_is_escape_sequence => {
                                let escape_char = match c {
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    '\\' => '\\',
                                    '0' => '\0',
                                    '"' => '"',
                                    _ => Err(TokenizerError::InvalidEscapeSequence(here_loc))?,
                                };
                                value.push(escape_char);
                                *next_is_escape_sequence = false;
                            }

                            // Prepare for escape sequences
                            '\\' => {
                                *next_is_escape_sequence = true;
                            }

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

            TokenizerState::DocComment(token) => {
                if here == '\n' {
                    self.tokens.push(token.clone());
                    self.state = TokenizerState::Idle
                } else {
                    let TokenKind::DocComment(ref mut s) = token.kind else {
                        unreachable!();
                    };
                    s.push(here);
                }
            }
        }

        self.advance();
        Ok(())
    }

    /// Converts the given string to a corresponding [`TokenKeyword`], or `None` if none exists.
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
            "mixin"   => Some(TokenKeyword::Mixin),
            "use"     => Some(TokenKeyword::Use),
            "return"  => Some(TokenKeyword::Return),
            _         => None,
        }
    }

    /// Steps the tokenizer until the entire source file has been tokenized (or an error occurs),
    /// then returns the tokens.
    pub fn tokenize(source_file: Rc<SourceFile>) -> Result<Vec<Token>, TokenizerError> {
        let chars = source_file.contents.chars().collect::<Vec<_>>();
        let mut tokenizer = Tokenizer::new(source_file.clone(), &chars[..]);

        while tokenizer.current_index < chars.len() {
            tokenizer.step()?;
        }

        // If the tokenizer was parsing an arbitrary-length token, shift it now
        if let TokenizerState::CollectingWhitespaceSeparated { ref mut token, .. } = tokenizer.state {
            // Keyword check
            if let TokenKind::Identifier(id) = &token.kind {
                if let Some(keyword) = Self::to_keyword(id) {
                    token.kind = TokenKind::Keyword(keyword);
                }
            }
            
            tokenizer.tokens.push(token.clone());
        }

        tokenizer.tokens.push(TokenKind::EndOfFile.at(tokenizer.here_loc()));

        Ok(tokenizer.tokens)
    }
}
