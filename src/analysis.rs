use std::ops::Index;
use std::fmt::Debug;

use crate::*;
use rustc_hash::{FxHashSet};

/// A bottom up statich analysis with shared data `A` along with
/// local data `A::Item` for each node.
#[derive(Debug, Clone)]
pub struct AnalyzedExpr<A: Analysis>{
    nodes: Vec<A::Item>,
    shared: A,
}

/// trait for implementing a bottom up static analysis
pub trait Analysis: Sized + Debug + Clone {
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
    
    /// analyze all ndoes up to the index `idx` (inclusive)
    pub fn analyze_to(&mut self, set: &ExprSet, idx: Idx) {
        assert_eq!(set.order, Order::ChildFirst);
        while self.nodes.len() <= idx {
            self.nodes.push(A::new(set.get(self.nodes.len()), self));
        }
    }

    /// analyze all nodes in an expression, picking up where you left off if this has been run before.
    /// Assumes nodes were only appended since the last time this was run on `set`.
    pub fn analyze(&mut self, set: &ExprSet) {
        self.analyze_to(set, set.len()-1)
    }

    /// calls analyze() then returns the analysis at the index you analyzed up to
    pub fn analyze_get(&mut self, e: Expr) -> &A::Item {
        self.analyze_to(e.set, e.idx);
        &self[e.idx]
    }
}

impl<A: Analysis> Index<Idx> for AnalyzedExpr<A> {
    type Output = A::Item;

    fn index(&self, idx: Idx) -> &Self::Output {
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

impl Analysis for &ExprCost {
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


#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct FreeVarAnalysis;
impl Analysis for FreeVarAnalysis {
    type Item = FxHashSet<i32>;
    fn new(e: Expr, analyzed: &AnalyzedExpr<Self>) -> Self::Item {
        let mut free: FxHashSet<i32> = Default::default();
        match e.node() {
            Node::IVar(_) => {},
            Node::Var(i) => {
                free.insert(*i);
            },
            Node::Prim(_) => {},
            Node::App(f, x) => {
                free.extend(analyzed[*f].iter());
                free.extend(analyzed[*x].iter());
            }
            Node::Lam(b) => {
                free.extend(analyzed[*b].iter()
                    .filter(|i| **i > 0)    
                    .map(|i| i - 1)
                );
            }
        }
        free
    }
}

#[derive(Debug, Clone)]
pub struct IVarAnalysis;
impl Analysis for IVarAnalysis {
    type Item = FxHashSet<i32>;
    fn new(e: Expr, analyzed: &AnalyzedExpr<Self>) -> Self::Item {
        let mut free: FxHashSet<i32> = Default::default();
        match e.node() {
            Node::IVar(i) => {
                free.insert(*i);
            },
            Node::Var(_) => {},
            Node::Prim(_) => {},
            Node::App(f, x) => {
                free.extend(analyzed[*f].iter());
                free.extend(analyzed[*x].iter());
            }
            Node::Lam(b) => {
                free.extend(analyzed[*b].iter());
            }
        }
        free
    }
}






