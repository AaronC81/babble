#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Location {
    pub start_index: usize,
    pub length: usize,
}

impl Location {
    pub fn new(start_index: usize, length: usize) -> Self {
        Location { start_index, length }
    }

    pub fn new_single(index: usize) -> Self {
        Self::new(index, 1)
    }
}
