use super::{Node, NodeKind, Literal, SugarNodeKind};

pub trait NodeWalk {
    /// Calls `func` on this node and all child nodes.
    fn walk_children(&mut self, func: &mut impl FnMut(&mut Node));
}

impl NodeWalk for Node {
    fn walk_children(&mut self, func: &mut impl FnMut(&mut Node)) {
        match &mut self.kind {
            NodeKind::StatementSequence(seq) => {
                for node in seq {
                    func(node);
                }
            },
            NodeKind::SendMessage { receiver, components } => {
                func(receiver);
                for node in components.child_nodes_mut() {
                    func(node);
                }
            },
            NodeKind::Assignment { target, value } => {
                func(target);
                func(value);
            },
            NodeKind::Block { body, parameters: _, captures: _ } => {
                func(body);
            },
            NodeKind::EnumVariant { enum_type, variant_name: _, components } => {
                func(enum_type);
                for node in components.child_nodes_mut() {
                    func(node);
                }
            },
            NodeKind::ImplBlock { target, body } => {
                func(target);
                func(body);
            },
            NodeKind::FuncDefinition { parameters, body, is_static: _, documentation: _ } => {
                for node in parameters.child_nodes_mut() {
                    func(node);
                }
                func(body);
            },
            NodeKind::Use(mixin) => {
                func(mixin);
            }
            NodeKind::Array(items) => {
                for item in items {
                    func(item);
                }
            }

            NodeKind::Sugar(SugarNodeKind::Return(value)) => {
                func(value);
            }

            | NodeKind::SelfAccess
            | NodeKind::EnumDefinition { name: _, variants: _ }
            | NodeKind::StructDefinition { name: _, instance_fields: _, static_fields: _ }
            | NodeKind::MixinDefinition { name: _ }
            | NodeKind::Identifier(_)
            | NodeKind::Literal(_) => (),
        }
    }
}
