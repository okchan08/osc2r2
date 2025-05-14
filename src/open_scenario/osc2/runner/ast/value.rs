#[allow(dead_code)]
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(super) enum Value {
    // TODO implement this enum
    #[default]
    None,
    Int(i64),
}
