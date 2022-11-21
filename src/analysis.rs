use crate::*;

/// A bottom up statich analysis with shared data `A` along with
/// local data `A::Item` for each node.
pub struct AnalyzedExpr<A: Analysis>{
    nodes: Vec<A::Item>,
    shared: A,
}

/// trait for implementing a bottom up static analysis
pub trait Analysis: Sized {
    type Item;
    /// Analyze the root of `e`, assuming all children of it have already
    /// been analyzed
    fn new(e: Expr, analyzed: &AnalyzedExpr<Self>) -> Self::Item;
}

impl<A: Analysis> AnalyzedExpr<A> {
    pub fn new(shared: A) -> Self {
        AnalyzedExpr {
            nodes: vec![],
            shared,
        }
    }
    /// analyze up to e.idx (inclusive), starting from the end up whatever we've already
    /// analyzed so far.
    /// Important: Assumes nothing that no nodes will ever be removed from the Expr we are analyzing, ie
    /// all old records of how Idxs correspond to Analysis items are still valid.
    pub fn update(&mut self, e: Expr) -> &A::Item {
        while self.nodes.len() <= e.idx {
            self.nodes.push(A::new(e.get(self.nodes.len()), self));
        }
        self.get(e.idx)
    }
    pub fn get(&self, idx: Idx) -> &A::Item {
        &self.nodes[idx]
    }
}


impl Analysis for () {
    type Item = ();
    fn new(_: Expr, _: &AnalyzedExpr<Self>) -> Self {
        ()
    }
}

impl Analysis for ExprCost {
    type Item = i32;
    fn new(e: Expr, analyzed: &AnalyzedExpr<Self>) -> Self::Item {
        match e.node() {
            Node::IVar(_) => analyzed.shared.cost_ivar,
            Node::Var(_) => analyzed.shared.cost_var,
            Node::Prim(p) => *analyzed.shared.cost_prim.get(p).unwrap_or(&analyzed.shared.cost_prim_default),
            Node::App(f, x) => {
                analyzed.shared.cost_app + analyzed.nodes[*f] + analyzed.nodes[*x] 
            }
            Node::Lam(b) => {
                analyzed.shared.cost_lam + analyzed.nodes[*b]
            }
        }
    }
}

pub struct DepthAnalysis;
impl Analysis for DepthAnalysis {
    type Item = usize;
    fn new(e: Expr, analyzed: &AnalyzedExpr<Self>) -> Self::Item {
        match e.node() {
            Node::IVar(_) => 1,
            Node::Var(_) => 1,
            Node::Prim(_) => 1,
            Node::App(f, x) => {
                1 + std::cmp::max(analyzed.nodes[*f], analyzed.nodes[*x])
            }
            Node::Lam(b) => {
                1 + analyzed.nodes[*b]
            }
        }
    }
}






