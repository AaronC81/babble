//! Represents metadata and contents of source files loaded into the interpreter.

use std::rc::Rc;

/// A location within a source file, spanning a range of characters from a starting point.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub source_file: Rc<SourceFile>,
    pub start_index: usize,
    pub length: usize,
}

impl Location {
    /// Constructs a new location within a source file.
    pub fn new(source_file: Rc<SourceFile>, start_index: usize, length: usize) -> Self {
        Location { source_file, start_index, length }
    }

    /// Constructs a new location within a source file, pointing to just a single character.
    pub fn new_single(source_file: Rc<SourceFile>, index: usize) -> Self {
        Self::new(source_file, index, 1)
    }

    /// Computes the line which this location can be found on.
    pub fn line_number(&self) -> usize {
        let mut current_line_number = 1;
        for (i, c) in self.source_file.contents.chars().enumerate() {
            if i == self.start_index { return current_line_number; }
            if c == '\n' { current_line_number += 1; }
        }

        unreachable!("file does not contain start index")
    }

    /// Returns the contents of the line which this location can be found on.
    pub fn line_contents(&self) -> String {
        let target_line_number = self.line_number();
        let mut current_line_number = 1;
        let mut buffer = "".to_string();
        for c in self.source_file.contents.chars() {
            if current_line_number == target_line_number {
                buffer.push(c);
            }

            if c == '\n' { current_line_number += 1; }
            if current_line_number > target_line_number {
                break;
            }
        }

        return buffer;
    }
}

/// A source file processed by the interpreter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    pub name: String,
    pub contents: String,
}

impl SourceFile {
    /// Constructs a new source file with the given name and contents.
    pub fn new(name: &str, contents: &str) -> Self {
        Self { name: name.into(), contents: contents.into() }
    }

    pub fn new_temp(contents: &str) -> Self {
        Self::new("<temp>", contents)
    }

    /// Transforms this into a reference.
    pub fn rc(self) -> Rc<Self> {
        Rc::new(self)
    }
}
