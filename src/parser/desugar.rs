//! Provides methods for translating [`NodeKind::Sugar`] instances into desugared code.

use super::{Node, NodeWalk, NodeKind, SugarNodeKind, SendMessageComponents, SendMessageParameter, BlockParameters};

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
                                    ("value".into(), SendMessageParameter::Parsed(value.clone())),
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
                            ("throw".into(), SendMessageParameter::Parsed(Box::new(Node {
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
                                        ("catchTag".into(), SendMessageParameter::Parsed(Box::new(Node {
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
                                ("valueOr".into(), SendMessageParameter::Parsed(Box::new(Node {
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
