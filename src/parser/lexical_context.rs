//! **Currently unused**, as a hold-over from how local variables used to be implemented.
//! 
//! Kept around in case it might be needed later!

use std::{rc::Rc, cell::RefCell};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexicalContext {
    parent: Option<LexicalContextRef>,
}

impl LexicalContext {
    pub fn new_top_level() -> Self {
        LexicalContext { parent: None }
    }

    pub fn new_with_parent(parent: LexicalContextRef) -> Self {
        LexicalContext { parent: Some(parent) }
    }

    pub fn rc(self) -> LexicalContextRef {
        Rc::new(RefCell::new(self))
    }
}

pub type LexicalContextRef = Rc<RefCell<LexicalContext>>;
