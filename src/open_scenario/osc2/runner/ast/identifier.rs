#![allow(dead_code)]
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub(super) struct Identifier {
    pub(super) name: String,
}
