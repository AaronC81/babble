use crate::{tokenizer::Tokenizer, source::SourceFile};

use super::{Node, SendMessageComponents, NodeKind, Parser, SendMessageParameter};

#[test]
fn test_simple_parse() {
    let src = SourceFile::new_temp("32 add: 24.").rc();
    let parsed = Parser::parse(src.clone(), &Tokenizer::tokenize(src).unwrap()[..]).unwrap();
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
            (p1, SendMessageParameter::Parsed(box Node { kind: NodeKind::IntegerLiteral(24), .. })),
        ] if p1 == "add")
    ))
}

#[test]
fn test_comments() {
    let src = SourceFile::new_temp("32 add: 24. // This would be a syntax error, if not a comment!").rc();
    Parser::parse(src.clone(), &Tokenizer::tokenize(src).unwrap()[..]).unwrap();
}
