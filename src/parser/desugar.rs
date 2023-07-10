//! Provides methods for translating [`NodeKind::Sugar`] instances into desugared code.

use super::{Node, NodeWalk, NodeKind, SugarNodeKind, SendMessageComponents, SendMessageParameter, BlockParameters, NodeFactory, Literal};

/// Desugars all child sugar nodes of the given node.
pub fn desugar_all(root: &mut Node) {
    desugar_return(root);
    desugar_simple(root);
}

/// Desugars uses of the `return` keyword.
/// 
/// A function like the following:
/// 
/// ```
/// func x {
///   return 2
/// }
/// ```
/// 
/// Is desugared into:
/// 
/// ```
/// func x {
///     ___desugarEarlyReturnValue = Match#Miss.
///     ___desugarNormalReturnValue = Program catchTag: [ |___desugarEarlyReturnTag|
///         ___desugarEarlyReturnValue = Match#Hit value: 2.
///         Program throw: ___desugarEarlyReturnTag.
///     ].
///     ___desugarEarlyReturnValue valueOr: ___desugarNormalReturnValue
/// }
/// ```
pub fn desugar_return(root: &mut Node) {
    let mut state = DesugarReturnState {
        return_used_by_current_func: false
    };
    desugar_return_internal(root, &mut state);
}

struct DesugarReturnState {
    return_used_by_current_func: bool,
}

const EARLY_RETURN_VALUE: &str = "___desugarEarlyReturnValue";
const EARLY_RETURN_TAG: &str = "___desugarEarlyReturnTag";
const NORMAL_RETURN_VALUE: &str = "___desugarNormalReturnValue";

fn desugar_return_internal(node: &mut Node, state: &mut DesugarReturnState) {
    if let NodeKind::Sugar(SugarNodeKind::Return(value)) = &node.kind {
        state.return_used_by_current_func = true;

        *node = Node {
            kind: NodeKind::StatementSequence(vec![
                // Assign early return value...
                Node {
                    kind: NodeKind::Assignment {
                        target: Box::new(Node {
                            kind: NodeKind::Identifier(EARLY_RETURN_VALUE.to_string()),
                            ..node.clone()
                        }),
                        value: Box::new(Node {
                            kind: NodeKind::EnumVariant {
                                enum_type: Box::new(Node {
                                    kind: NodeKind::Identifier("Match".to_string()),
                                    ..node.clone()
                                }),
                                variant_name: "Hit".into(),
                                components: SendMessageComponents::Parameterised(vec![
                                    ("value".into(), SendMessageParameter::CallArgument(value.clone())),
                                ])
                            },
                            ..node.clone()
                        }),
                    },
                    ..node.clone()
                },

                // Then throw the tag
                Node {
                    kind: NodeKind::SendMessage {
                        receiver: Box::new(Node {
                            kind: NodeKind::Identifier("Program".to_string()),
                            ..node.clone()
                        }),
                        components: SendMessageComponents::Parameterised(vec![
                            ("throw".into(), SendMessageParameter::CallArgument(Box::new(Node {
                                kind: NodeKind::Identifier(EARLY_RETURN_TAG.to_string()),
                                ..node.clone()
                            }))),
                        ])
                    },
                    ..node.clone()
                }
            ]),
            ..node.clone()
        }
    }

    node.walk_children(&mut |n| desugar_return_internal(n, state));

    if let NodeKind::FuncDefinition { body, .. } = &mut node.kind {
        if state.return_used_by_current_func {
            // Wrap the body in return infrastructure
            *body = Box::new(Node {
                kind: NodeKind::StatementSequence(vec![
                    // Initialise early return value as a miss
                    Node {
                        kind: NodeKind::Assignment {
                            target: Box::new(Node {
                                kind: NodeKind::Identifier(EARLY_RETURN_VALUE.to_string()),
                                ..*body.clone()
                            }),
                            value: Box::new(Node {
                                kind: NodeKind::EnumVariant {
                                    enum_type: Box::new(Node {
                                        kind: NodeKind::Identifier("Match".to_string()),
                                        ..*body.clone()
                                    }),
                                    variant_name: "Miss".into(),
                                    components: SendMessageComponents::Blank,
                                },
                                ..*body.clone()
                            }),
                        },
                        ..*body.clone()
                    },

                    // Execute original body inside a `catchTag:` call, assigning to normal return
                    // value
                    Node {
                        kind: NodeKind::Assignment {
                            target: Box::new(Node {
                                kind: NodeKind::Identifier(NORMAL_RETURN_VALUE.to_string()),
                                ..*body.clone()
                            }),
                            value: Box::new(Node {
                                kind: NodeKind::SendMessage {
                                    receiver: Box::new(Node {
                                        kind: NodeKind::Identifier("Program".to_string()),
                                        ..*body.clone()
                                    }),
                                    components: SendMessageComponents::Parameterised(vec![
                                        ("catchTag".into(), SendMessageParameter::CallArgument(Box::new(Node {
                                            kind: NodeKind::Block {
                                                body: body.clone(),
                                                captures: vec![],
                                                parameters: BlockParameters::Named(vec![EARLY_RETURN_TAG.into()]),
                                            },
                                            ..*body.clone()
                                        }))),
                                    ])
                                },
                                ..*body.clone()
                            }),
                        },
                        ..*body.clone()
                    },

                    // Return either the early return value or the normal return value, using
                    // `valueOr:` to unwrap the match
                    Node {
                        kind: NodeKind::SendMessage {
                            receiver: Box::new(Node {
                                kind: NodeKind::Identifier(EARLY_RETURN_VALUE.into()),
                                ..*body.clone()
                            }),
                            components: SendMessageComponents::Parameterised(vec![
                                ("valueOr".into(), SendMessageParameter::CallArgument(Box::new(Node {
                                    kind: NodeKind::Identifier(NORMAL_RETURN_VALUE.into()),
                                ..*body.clone()
                                }))),
                            ])
                        },
                        ..*body.clone()
                    }
                ]),
                ..*body.clone()
            });

            // Reset the flag for the next method definition
            state.return_used_by_current_func = false;
        }
    }
}

/// Desugars simple syntactical sugars which only need to look at a single node at a time.
/// 
/// These are...
/// 
/// # Shorthand blocks
/// 
/// A call like the following:
/// 
/// ```
/// %{ "a" "b" "c" } sortBy: &length
/// ```
/// 
/// Is desugared into:
/// 
/// ```
/// %{ "a" "b" "c" } sortBy: [ |x| x length ]
/// ```
/// 
/// # Binary messages
/// 
/// A call like the following:
/// 
/// ```
/// 3 * 4 + 2
/// ```
/// 
/// Is desugared into:
/// 
/// ```
/// (3 mul: 4) add: 2
/// ```
/// 
/// (Babble does not use operator precedence)
/// 
/// # Shorthand variant constructors
/// 
/// This syntax:
/// 
/// ```
/// #Hit
/// ```
/// 
/// Is desugared into:
/// 
/// ```
/// (Reflection instanceType: self)#Hit
/// ```
/// 
/// # String interpolation
/// 
/// This syntax:
/// 
/// ```
/// "2 + 2 is {2 + 2}!"
/// ```
/// 
/// Is desugared into:
/// 
/// ```
/// String interpolate: #{
///     "2 + 2 is ".
///     (2 + 2).
///     "!".
/// }
/// ```
pub fn desugar_simple(root: &mut Node) {
    // Shorthand blocks
    if let NodeKind::Sugar(SugarNodeKind::ShorthandBlock(method)) = &root.kind {
        *root = Node {
            kind: NodeKind::Block {
                body: Box::new(Node {
                    kind: NodeKind::SendMessage {
                        receiver: Box::new(Node {
                            kind: NodeKind::Identifier(SHORTHAND_BLOCK_PARAMETER.into()),
                            ..root.clone()
                        }),
                        components: SendMessageComponents::Unary(method.clone())
                    },
                    ..root.clone()
                }),
                parameters: BlockParameters::Named(vec![SHORTHAND_BLOCK_PARAMETER.into()]),
                captures: vec![],
            },
            ..root.clone()
        }
    }

    // Binary messages
    if let NodeKind::Sugar(SugarNodeKind::BinaryMessage { left, right, op }) = &root.kind {
        *root = Node {
            kind: NodeKind::SendMessage {
                receiver: left.clone(),
                components: SendMessageComponents::Parameterised(vec![
                    (op.parameter_name().into(), SendMessageParameter::CallArgument(right.clone()))
                ]),
            },
            ..root.clone()
        }
    }

    // Shorthand variant constructors
    if let NodeKind::Sugar(SugarNodeKind::ShorthandVariantConstructor) = &root.kind {
        *root = Node {
            kind: NodeKind::SendMessage {
                receiver: Box::new(Node {
                    kind: NodeKind::Identifier("Reflection".to_string()),
                    ..root.clone()
                }),
                components: SendMessageComponents::Parameterised(vec![
                    ("instanceType".into(), SendMessageParameter::CallArgument(Box::new(Node {
                        kind: NodeKind::SelfAccess,
                        ..root.clone()
                    }))),
                ])
            },
            ..root.clone()
        }
    }

    // String interpolation
    if let NodeKind::Sugar(SugarNodeKind::StringInterpolation(parts)) = &root.kind {
        *root = Node {
            kind: NodeKind::SendMessage {
                receiver: Box::new(Node {
                    kind: NodeKind::Identifier("String".to_string()),
                    ..root.clone()
                }),
                components: SendMessageComponents::Parameterised(vec![
                    ("interpolate".into(), SendMessageParameter::CallArgument(Box::new(Node {
                        kind: NodeKind::Array(parts.clone()),
                        ..root.clone()
                    }))),
                ])
            },
            ..root.clone()
        }
    }

    // Pattern blocks
    if let NodeKind::Sugar(SugarNodeKind::PatternBlock { block, patterns, fatal }) = &root.kind {
        const MATCH_FAIL_TAG: &str = "___desugarMatchFailTag";

        let NodeKind::Block { body, parameters, captures } = &block.kind else {
            unreachable!("PatternBlock constructed from non-block node")
        };

        // The block should have the same number of parameters as patterns
        let BlockParameters::Named(parameter_names) = parameters else {
            unreachable!("non-named parameters in PatternBlock")
        };
        if parameter_names.len() != patterns.len() {
            unreachable!("mismatch in PatternBlock parameters vs patterns len")
        }
    
        // Generate something to do if the patterns don't match, based on whether the block is
        // fatal (![...]) or returns a `Match` (?[...])
        let factory = NodeFactory::new(&root.location, &root.context);
        let no_match = if *fatal {
            NodeKind::SendMessage {
                receiver: factory.build_boxed(NodeKind::Identifier("Program".into())),
                components: factory.build_args([
                    ("error", factory.build(NodeKind::Literal(Literal::String("pattern did not match".into()))))
                ]),
            }
        } else {
            NodeKind::SendMessage {
                receiver: factory.build_boxed(NodeKind::Identifier("Program".into())),
                components: factory.build_args([
                    ("throw", factory.build(NodeKind::Identifier(MATCH_FAIL_TAG.into())))
                ]),
            }
        };

        // Generate pattern matching code
        let mut nodes = parameter_names.iter()
            .zip(patterns.iter())
            .map(|(param, pattern)| {
                let candidate = factory.build(NodeKind::Identifier(param.into()));
                pattern.desugar(&candidate, &param, &no_match, factory.location, factory.context)
            })
            .flatten()
            .collect::<Vec<_>>();

        // Add all other nodes from the definition of the block
        let NodeKind::StatementSequence(seq) = &body.kind else {
            unreachable!("block body is not a statement sequence");
        };
        nodes.extend(seq.iter().cloned());

        // If generating a non-fatal pattern, wrap all of the nodes in a call to `Match byThrowing:`
        if !fatal {
            nodes = vec![
                factory.build(NodeKind::SendMessage {
                    receiver: factory.build_boxed(NodeKind::Identifier("Match".into())),
                    components: factory.build_args([
                        ("byThrowing", factory.build(NodeKind::Block {
                            body: factory.build_boxed(NodeKind::StatementSequence(nodes)),
                            parameters: BlockParameters::Named(vec![MATCH_FAIL_TAG.into()]),
                            captures: vec![],
                        }))
                    ]),
                })
            ]
        }

        // Finally, assign new block
        root.kind = NodeKind::Block {
            body: factory.build_boxed(NodeKind::StatementSequence(nodes)),
            parameters: parameters.clone(),
            captures: captures.clone(),
        }
    }

    root.walk_children(&mut desugar_simple);
}

const SHORTHAND_BLOCK_PARAMETER: &str = "___desugarShorthandBlockParameter";
