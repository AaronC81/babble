use askama::Template;
use comrak::{markdown_to_html, ComrakOptions};

use crate::interpreter::{Interpreter, MethodLocality, TypeData, DocumentationState, MethodVisibility};

struct TypeDocumentation {
    pub id: String,
    pub data: TypeData,
    pub used_mixins: Vec<String>,
    pub methods: Vec<MethodDocumentation>,
    pub description: Option<String>,
}

struct MethodParsedDocumentation {
    pub description: String,
    pub parameters: Vec<(String, String)>,
    pub return_value: Option<String>,
    pub magic: bool,
}

struct MethodDocumentation {
    pub name: String,
    pub locality: MethodLocality,
    pub visibility: MethodVisibility,
    pub parsed: Option<MethodParsedDocumentation>,
}

#[derive(Template)]
#[template(path = "index.html")]
struct HtmlDocumentationTemplate {
    types: Vec<TypeDocumentation>,
}

trait DocumentationTemplate {
    fn from_types(types: Vec<TypeDocumentation>) -> Self;
}

impl DocumentationTemplate for HtmlDocumentationTemplate {
    fn from_types(types: Vec<TypeDocumentation>) -> Self {
        Self { types }
    }
}

impl MethodParsedDocumentation {
    pub fn parse(string: &str) -> Self {
        let mut result = MethodParsedDocumentation {
            description: "".into(),
            parameters: vec![],
            return_value: None,
            magic: false,
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
            } else if line.starts_with("@magic") {
                result.magic = true;
            } else {
                result.description.push_str(line);
                result.description.push_str("\n");
            }
        }

        // Parse each field as Markdown
        let options = ComrakOptions::default();
        result.description = markdown_to_html(&result.description, &options);
        for (_, param) in &mut result.parameters {
            *param = markdown_to_html(param, &options);
        }
        if let Some(ref mut ret) = result.return_value {
            *ret = markdown_to_html(ret, &options);
        }

        result
    }
}

/// Iterates over the type repository for the given interpreter state, and generates a page of
/// documentation for the types in it.
fn build_documentation_objects<T: DocumentationTemplate>(interpreter: &Interpreter) -> T {
    let mut types = interpreter.types.clone();
    types.sort_by_key(|t| t.borrow().id.clone());

    let mut type_docs = vec![];
    for t in &types {
        let t = t.borrow();

        if let DocumentationState::Hidden = t.documentation {
            continue;
        }

        // Build type documentation
        let data = t.data.clone();
        let used_mixins = t.used_mixins.iter().map(|m| m.borrow().id.clone()).collect();

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

        let mut method_docs = vec![];
        // Generate documentation for each
        for (m, l) in all_methods {
            let doc = match m.documentation {
                DocumentationState::Documented(ref docs) => Some(docs),
                DocumentationState::Undocumented => None,
                DocumentationState::Hidden => continue,
            };

            method_docs.push(MethodDocumentation {
                name: m.name.clone(),
                locality: l,
                visibility: m.visibility,
                parsed: doc.as_ref().map(|d| MethodParsedDocumentation::parse(d)),
            });
        }

        // Parse description as Markdown
        let options = ComrakOptions::default();
        let mut description: Option<String> = t.documentation.clone().into();
        description = description.as_ref()
            .map(|d| markdown_to_html(&d.split("\n").map(|l| l.trim()).collect::<Vec<_>>().join("\n"), &options));

        type_docs.push(TypeDocumentation {
            id: t.id.clone(),
            data,
            used_mixins,
            methods: method_docs,
            description,
        });
    }
    
    T::from_types(type_docs)
}

pub fn generate_html_documentation(interpreter: &Interpreter) -> String {
    let template = build_documentation_objects::<HtmlDocumentationTemplate>(interpreter);
    template.render().unwrap()
}
