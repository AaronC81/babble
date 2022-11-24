/// How an item is documented.
#[derive(Debug, Clone)]
pub enum DocumentationState {
    /// The item has documentation comments.
    Documented(String),

    /// The item does not have documentation comments, but is still visible when generating
    /// documentation.
    Undocumented,

    /// The item explicitly should not appear in documentation.
    Hidden,
}

impl Into<Option<String>> for DocumentationState {
    fn into(self) -> Option<String> {
        match self {
            DocumentationState::Documented(s) => Some(s),
            _ => None,
        }
    }
}
