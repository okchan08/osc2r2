use super::open_drive;
use super::road;
use roxmltree;
use std::fs;
use std::path::Path;

pub fn parse_open_drive<T: AsRef<Path>>(path: T) -> open_drive::OpenDrive {
    let s = fs::read(path.as_ref()).unwrap();
    let binding = String::from_utf8(s).unwrap();
    let contents = binding.as_str();
    let doc = roxmltree::Document::parse(&contents).unwrap();
    let mut node = doc.root_element();

    let _roads = road::Road::parse_roads(&mut node);

    open_drive::OpenDrive {}
}
