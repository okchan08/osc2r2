use serde::{Deserialize, Serialize};
use std::fmt;

// Source code location
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Location {
    pub(super) row: u32,
    pub(super) column: u32,
}

impl Location {
    pub fn new(row: usize, column: usize) -> Self {
        Location {
            row: row.try_into().expect("Location::row over u32"),
            column: column.try_into().expect("Location::column over u32"),
        }
    }

    pub fn fmt_with(&self, f: &mut fmt::Formatter, e: &impl fmt::Display) -> fmt::Result {
        write!(f, "{} at line {} column {}", e, self.row, self.column)
    }

    /// Current line
    pub fn row(&self) -> usize {
        self.row as usize
    }

    /// Current column
    pub fn column(&self) -> usize {
        self.column as usize
    }

    pub fn reset(&mut self) {
        self.row = 1;
        self.column = 0;
    }

    pub fn go_right(&mut self) {
        self.column += 1;
    }

    pub fn go_left(&mut self) {
        self.column -= 1;
    }

    pub fn newline(&mut self) {
        self.row += 1;
        self.column = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::Location;

    #[test]
    fn test_gt() {
        assert!(Location::new(1, 2) > Location::new(1, 1));
        assert!(Location::new(2, 1) > Location::new(1, 1));
        assert!(Location::new(2, 1) > Location::new(1, 2));
        assert!(Location::new(2, 2) == Location::new(2, 2));
    }

    #[test]
    fn test_lt() {
        assert!(Location::new(1, 1) < Location::new(1, 2));
        assert!(Location::new(1, 1) < Location::new(2, 1));
        assert!(Location::new(1, 2) < Location::new(2, 1));
    }
}
