use crate::interpreter::{Interpreter, MethodLocality, TypeData, DocumentationState};

struct ParsedDocumentationComments {
    pub description: String,
    pub parameters: Vec<(String, String)>,
    pub return_value: Option<String>,
}

impl ParsedDocumentationComments {
    pub fn parse(string: &str) -> Self {
        let mut result = ParsedDocumentationComments {
            description: "".into(),
            parameters: vec![],
            return_value: None,
        };

        for line in string.split("\n") {
            let line = line.trim();

            if line.starts_with("@param") {
                let line = line.trim_start_matches("@param").trim();
                let mut parts = line.splitn(2, " ");
                let name = parts.next().unwrap().trim();
                let description = parts.next().unwrap().trim();
                result.parameters.push((name.into(), description.into()));
            } else if line.starts_with("@returns") {
                let line = line.trim_start_matches("@returns").trim();
                result.return_value = Some(line.into());
            } else {
                result.description.push_str(line);
                result.description.push_str("\n");
            }
        }

        result
    }
}

/// Iterates over the type repository for the given interpreter state, and generates a page of
/// documentation for the types in it.
pub fn generate_documentation(interpreter: &Interpreter) -> String {
    let mut types = interpreter.types.clone();
    types.sort_by_key(|t| t.borrow().id.clone());

    let mut output = "".to_string();
    output.push_str("# Table of Contents\n\n");
    for t in &types {
        let t = t.borrow();

        // Links only work if they're lowercase in VS Code
        output.push_str(&format!("- [{}](#{})\n", t.id, t.id.to_lowercase()));
    }
    output.push_str("\n---\n\n");

    for t in &types {
        let t = t.borrow();
        output.push_str(&format!("# {}\n\n", t.id));

        // Output some info about the type itself
        match &t.data {
            TypeData::Fields(fields) => {
                output.push_str("**Definition**: Struct with fields:\n\n");
                for name in fields {
                    output.push_str(&format!("- `{}`\n", name));
                }
            },
            TypeData::Variants(variants) => {
                output.push_str("**Definition**: Enum with variants:\n\n");
                for variant in variants {
                    if variant.fields.is_empty() {
                        output.push_str(&format!("- `{}`\n", variant.name));
                    } else {
                        output.push_str(&format!("- `{}`, fields:\n", variant.name));
                        for name in &variant.fields {
                            output.push_str(&format!("  - `{}`\n", name));
                        }
                    }
                }
            },
            TypeData::Mixin => output.push_str("**Definition**: Mixin\n\n"),
            TypeData::Empty => output.push_str("**Definition**: Primitive\n\n"),
        };

        // Gather all instance and static methods
        let mut all_methods = t.methods.iter().cloned()
            .map(|m| (m, MethodLocality::Instance))
            .chain(
                t.static_methods.iter()
                    .cloned()
                    .map(|m| (m, MethodLocality::Static))
            )
            .collect::<Vec<_>>();
        all_methods.sort_by_key(|(m, _)| m.name.clone());

        // Generate documentation for each
        for (m, l) in all_methods {
            let doc = match m.documentation {
                DocumentationState::Documented(ref docs) => Some(docs),
                DocumentationState::Undocumented => None,
                DocumentationState::Hidden => continue,
            };

            output.push_str(&match l {
                MethodLocality::Instance => format!("## `{}`\n\n", m.name),
                MethodLocality::Static => format!("## `static {}`\n\n", m.name),
            });
            if let Some(doc) = doc {
                let parsed = ParsedDocumentationComments::parse(doc);

                output.push_str(&format!("{}\n\n", parsed.description));
                if !parsed.parameters.is_empty() {
                    output.push_str(&format!("**Parameters:**\n\n"));
                    for (name, description) in parsed.parameters {
                        output.push_str(&format!("- `{}` {}\n", name, description));
                    }
                    output.push_str(&format!("\n"));
                }

                if let Some(return_value) = parsed.return_value {
                    output.push_str(&format!("**Returns:** {}\n\n", return_value));
                }
            } else {
                output.push_str("_Undocumented._\n\n");
            }
        }

        output.push_str("---\n\n");
    }
    output
}
