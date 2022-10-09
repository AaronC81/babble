use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub source_file: Rc<SourceFile>,
    pub start_index: usize,
    pub length: usize,
}

impl Location {
    pub fn new(source_file: Rc<SourceFile>, start_index: usize, length: usize) -> Self {
        Location { source_file, start_index, length }
    }

    pub fn new_single(source_file: Rc<SourceFile>, index: usize) -> Self {
        Self::new(source_file, index, 1)
    }

    pub fn line_number(&self) -> usize {
        let mut current_line_number = 1;
        for (i, c) in self.source_file.contents.chars().enumerate() {
            if i == self.start_index { return current_line_number; }
            if c == '\n' { current_line_number += 1; }
        }

        unreachable!("file does not contain start index")
    }

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    pub name: String,
    pub contents: String,
}

impl SourceFile {
    pub fn new(name: &str, contents: &str) -> Self {
        Self { name: name.into(), contents: contents.into() }
    }

    pub fn new_temp(contents: &str) -> Self {
        Self::new("<temp>", contents)
    }

    pub fn rc(self) -> Rc<Self> {
        Rc::new(self)
    }
}
