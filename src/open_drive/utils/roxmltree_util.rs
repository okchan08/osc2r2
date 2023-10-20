use std::{fmt::Debug, str::FromStr};

use roxmltree;

pub fn parse_node_attribute<T>(node: &roxmltree::Node, name: &str, default_val: T) -> T
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    match node.attribute(name) {
        Some(res) => res.parse::<T>().unwrap(),
        None => default_val,
    }
}
