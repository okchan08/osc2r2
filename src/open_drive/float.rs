//use std::hash::{Hash, Hasher};
//
//#[derive(PartialEq, Debug, Clone, Copy, PartialOrd, Ord)]
//pub struct F64(pub f64);
//
//impl Eq for F64 {}
//
//impl Hash for F64 {
//    fn hash<H>(&self, state: &mut H)
//    where
//        H: Hasher,
//    {
//        self.0.to_bits().hash(state)
//    }
//}
//
