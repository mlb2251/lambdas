use crate::*;

/// A node in an ZPath
/// Ord: Func < Body < Arg
#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum ZNode {
    // * order of variants here is important because the derived Ord will use it
    Func, // zipper went into the function, so Idx is the arg
    Body, 
    Arg, // zipper went into the arg, so Idx is the function
}

// pub type Zip = Vec<ZNode>;

/// "zipper Idx" each unique zipper gets referred to by its zipper Idx
pub type ZId = usize;


/// a zid referencing a specific ZPath and a #i index
#[derive(Debug,Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct LabelledZId {
    pub zid: ZId,
    pub ivar: usize // which #i argument this is, which also corresponds to args[i] ofc
}

impl LabelledZId {
    pub fn new(zid: ZId, ivar: usize) -> LabelledZId {
        LabelledZId { zid, ivar }
    }
}

impl<'a> Expr<'a> {
    pub fn zip(&self, zipper: &[ZNode]) -> Self {
        let mut e = *self;
        for znode in zipper {
            e = match znode {
                ZNode::Func => e.left(),
                ZNode::Body => e.body(),
                ZNode::Arg => e.right(),
            }
        }
        e
    } 
}









