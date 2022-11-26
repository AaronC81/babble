//! Provides _capture analysis_, a phase after parsing which determines which local variables are
//! captured by blocks.

use crate::parser::NodeKind;

use super::{Node, traits::NodeWalk, BlockParameters};

/// A frame used in the capture analysis stack.
struct AnalysisStackFrame {
    assignments: Vec<String>,
    captures: Vec<String>,
}

impl AnalysisStackFrame {
    /// Creates a blank stack frame.
    fn new() -> Self {
        Self {
            assignments: vec![],
            captures: vec![],
        }
    }

    /// Checks whether this frame _provides_ a local, such that it can accessed with the given name
    /// - either by creating it with an assignment, or by capturing it from an outer scope.
    fn provides(&self, id: &String) -> bool {
        self.assignments.contains(id) || self.captures.contains(id)
    }
}

/// The entry point for capture analysis.
/// 
/// Given a root parsed node, constructs a new analysis stack, searches it for variable captures,
/// and populates capture information on any blocks found.
pub fn populate_captures(root: &mut Node) {
    let mut stack = vec![AnalysisStackFrame::new()];
    handle_node(root, &mut stack)
}

/// The recursive method which analyses nodes for captures.
fn handle_node(node: &mut Node, stack: &mut Vec<AnalysisStackFrame>) {
    // Before iterating into the children, look at what this node is
    match &node.kind {
        // Identifiers read a variable that we either should have declared or captured
        NodeKind::Identifier(id) => {
            // If we haven't declared it, but can't find somewhere to capture from - just don't
            // do anything. A runtime error is fine for this (it's an interpreted language,
            // after all), and more than likely it refers to a class or something.
            try_propagate_capture(id, stack);
        }

        // Similarly, assignments write to such a variable
        NodeKind::Assignment { target, .. } => {
            // If we're assigning to a local variable, grab that out of the target
            if let NodeKind::Identifier(id) = &target.kind {
                if !try_propagate_capture(id, stack) {
                    stack.last_mut().unwrap().assignments.push(id.clone());
                }
            };
        }

        // Blocks introduce a new frame onto our stack
        NodeKind::Block { parameters, .. } => {
            let mut frame = AnalysisStackFrame::new();
            // Ensure that we don't try to capture our own parameters
            frame.assignments.extend(match parameters {
                BlockParameters::Named(names) => names.clone(),
                BlockParameters::All(name) => vec![name.clone()],
                BlockParameters::Patterned { patterns, .. } => patterns.iter()
                    .flat_map(|p| p.all_bindings())
                    .collect(),
            });
            stack.push(frame);
        },

        // Function definitions also introduce a frame, so that we can capture parameters
        NodeKind::FuncDefinition { parameters, .. } => {
            let mut frame = AnalysisStackFrame::new();
            frame.assignments.extend(parameters.defined_internal_names());
            stack.push(frame);
        }

        // We're not bothered about anything else
        _ => (),
    }

    // Deal with children
    node.walk_children(&mut |n| handle_node(n, stack));

    // If the node was a block, then we can pop the stack and deal with any captures gathered for it
    if let NodeKind::Block { captures, .. } = &mut node.kind {
        let frame = stack.pop().unwrap();
        captures.extend(frame.captures);
    }

    // If it was a function definition, just pop the stack
    if let NodeKind::FuncDefinition { .. } = node.kind {
        stack.pop().unwrap();
    }
}

/// Given an identifier and an analysis stack, seek up the stack for something which provides a
/// local variable with that name.
/// 
/// If one is found, adds captures to each frame recursively between the top-most and the frame
/// which provides the variable, then returns `true`.
/// 
/// Otherwise, returns `false`.
fn try_propagate_capture(id: &String, stack: &mut [AnalysisStackFrame]) -> bool {
    // If the variable is already available in the last (i.e. current) stack frame, we don't need to
    // do anything
    let current_frame = stack.last().unwrap();
    if current_frame.provides(id) {
        return true
    }

    // Otherwise, check from the second-to-last frame to the root
    let mut found_at_index = None;
    for (i, frame) in stack.iter_mut().enumerate().rev().skip(1) {
        if frame.provides(id) {
            found_at_index = Some(i);
            break;
        }
    }

    if let Some(found_at_index) = found_at_index {
        // We found the variable declared at stack[i]. Propagate captures from the frame after that
        // one, up to the last frame.
        for frame in stack.iter_mut().skip(found_at_index + 1) {
            frame.captures.push(id.clone());
        }
        true
    } else {
        // We couldn't find it!
        false
    }
}
