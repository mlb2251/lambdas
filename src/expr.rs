use std::collections::HashMap;
use std::fmt::{self, Formatter, Display, Debug};
use std::hash::Hash;
use std::ops::{Index, IndexMut, Range};
use serde::{Serialize, Deserialize};
use std::cmp::{min,max};
use string_cache::DefaultAtom as Symbol;

pub type Idx = usize;
pub const HOLE: Idx = usize::MAX;


/// A node of an untyped lambda calculus expression compatible with `egg` but also used more widely throughout this crate.
/// Note that there is no domain associated with this object. This makes it easy to run compression on
/// domains that don't have semantics yet, makes it easy to add new prims (eg learned functions), etc.
/// You'll have to specify a domain when you execute the expression, type check it, etc, but you can easily do
/// that at the time through generics on those functions.
/// 
/// Variants:
/// * Var(i): "$i" a debruijn index variable
/// * IVar(i): "#i" a debruijn index variable used by inventions (advantage: readability of inventions + less shifting required)
/// * App([f, x]): f applied to x
/// * Lam([body]): lambda abstraction referred to through $i Vars
/// * Prim(Symbol): primitive (eg functions, constants, all nonvariable leaf nodes)
/// * Programs(Vec<Id>): list of root nodes of the programs. There's just one of these at the top of the program tree
/// 
/// Note there is no AppLam construct. This is because AppLams are represented through the `AppLam` struct when it comes
/// to invention-finding, and they don't belong in Lambda because they never actually show up within programs (theyre only
/// ever used in passing at the top level when constructing inventions) 
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Node where
{
    Prim(Symbol), // primitive (eg functions, constants, all nonvariable leaf nodes)
    Var(i32), // db index ($i)
    IVar(i32),
    App(Idx,Idx), // f, x
    Lam(Idx), // body
}

/// An untyped lambda calculus expression, much like `egg::RecExpr` but with a public `nodes` field
/// and many attached functions. See `Lambda` for details on the individual nodes.
/// 
/// Creation:
/// * From<RecExpr> is implemented (and vis versa) for interop with `egg`
/// * Expr::new() directly constructs an Expr from a Vec<Lambda>
/// * Expr::prim(Symbol), Expr::app(Expr,Expr), etc let you construct Exprs from other Exprs
/// * Expr::from_curried(&str) parses from a curried string like (app (app + 3) 4)
/// * Expr::from_uncurried(&str) parses from an uncurried string like (+ 3 4)
/// 
/// Displaying an expression or subexpression:
/// * fmt::Display is implemented to return an uncurried string like (+ 3 4)
/// * Expr::to_curried_string(Option<Id>) returns a curried string like (app (app + 3) 4) rooted at the Id if given
/// * Expr::to_uncurried_string(Option<Id>) returns an uncurried string like (+ 3 4) rooted at the Id if given
/// * Expr::save() lets you save an image of the expr to a file
/// 
/// Creating a subexpression:
/// * Expr::cloned_subexpr(Id) returns the subexpression rooted at the Id. Generally you want to avoid this because
///   most methods can get by just fine by taking a parent Expr and a child Id without the need for all this cloning.
///   Importantly all Id indexing should be preserved just fine since this is implemented through truncating the underlying vector.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExprSet {
    pub nodes: Vec<Node>,
    pub spans: Option<Vec<Range<Idx>>>,
    pub order: Order,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Order {
    ChildFirst,
    ParentFirst,
    Any
}

#[derive(Clone,Copy, Debug)]
pub struct Expr<'a> {
    pub set: &'a ExprSet,
    pub idx: Idx 
}

#[derive(Debug)]
pub struct ExprMut<'a> {
    pub set: &'a mut ExprSet,
    pub idx: Idx 
}

impl Index<Idx> for ExprSet {
    type Output = Node;
    #[inline(always)]
    fn index(&self, idx: Idx) -> &Self::Output {
        &self.nodes[idx]
    }
}
impl IndexMut<Idx> for ExprSet {
    #[inline(always)]
    fn index_mut(&mut self, idx: Idx) -> &mut Self::Output {
        &mut self.nodes[idx]
    }
}
impl Index<Range<Idx>> for ExprSet {
    type Output = [Node];
    #[inline(always)]
    fn index(&self, idx: Range<Idx>) -> &Self::Output {
        &self.nodes[idx]
    }
}
impl IndexMut<Range<Idx>> for ExprSet {
    #[inline(always)]
    fn index_mut(&mut self, idx: Range<Idx>) -> &mut Self::Output {
        &mut self.nodes[idx]
    }
}

impl ExprSet {
    pub fn empty(order: Order, spans: bool) -> ExprSet {
        let spans = if spans { Some(vec![]) } else { None };
        ExprSet { nodes: vec![], spans, order }
    }
    pub fn add(&mut self, node: Node) -> Idx {
        let idx = self.nodes.len();
        if let Some(spans) = &mut self.spans {
            let span = match node {
                Node::Var(_) | Node::Prim(_) | Node::IVar(_) => idx .. idx+1,
                Node::App(f, x) => min(min(spans[f].start,spans[x].start),idx) .. max(max(spans[f].end,spans[x].end),idx+1),
                Node::Lam(b) => min(spans[b].start,idx) .. max(spans[b].end,idx+1)
            };
            spans.push(span);
        }
        self.nodes.push(node);

        debug_assert!(self.get(idx).node_order_safe());
        idx
    }
    #[inline(always)]
    pub fn get(&self, idx: Idx) -> Expr {
        Expr { set: self, idx }
    }
    #[inline(always)]
    pub fn hole(&self) -> Expr {
        Expr { set: self, idx: HOLE }
    }
    #[inline(always)]
    pub fn get_mut(&mut self, idx: Idx) -> ExprMut {
        ExprMut { set: self, idx }
    }
    pub fn len(&self) -> usize {
        self.nodes.len()
    }
    pub fn truncate(&mut self, len: usize) {
        self.nodes.truncate(len);
    }
    pub fn iter(&self) -> impl ExactSizeIterator<Item=Idx> {
        (0..self.nodes.len()).into_iter()
    }
}


impl<'a> Expr<'a> {
    #[inline(always)]
    pub fn get(&self, idx: Idx) -> Self {
        Self { set: self.set, idx }
    }
    #[inline(always)]
    pub fn get_node(&'a self, idx: Idx) -> &'a Node {
        &self.set[idx]
    }
    #[inline(always)]
    pub fn node(&self) -> &Node {
        &self.set[self.idx]
    }
    pub fn get_span(&self) -> Option<Range<Idx>> {
        self.set.spans.as_ref().map(|spans| spans.get(self.idx).unwrap().clone())
    }
    pub fn iter_span(&self) -> impl ExactSizeIterator<Item=Idx> {
        self.get_span().unwrap().into_iter()
    }
    pub fn cost_span(&self, cost_fn: &ExprCost) -> i32 {
        let res = self.iter_span().map(|i|
            match self.set.get(i).node() {
                Node::IVar(_) => cost_fn.cost_ivar,
                Node::Var(_) => cost_fn.cost_var,
                Node::Prim(p) => *cost_fn.cost_prim.get(p).unwrap_or(&cost_fn.cost_prim_default),
                Node::App(_, _) => cost_fn.cost_app,
                Node::Lam(_) => cost_fn.cost_lam,
            }).sum::<i32>();
        debug_assert_eq!(res, self.cost_rec(cost_fn));
        res
    }

    pub fn cost_rec(&self, cost_fn: &ExprCost) -> i32 {
        match self.node() {
            Node::IVar(_) => cost_fn.cost_ivar,
            Node::Var(_) => cost_fn.cost_var,
            Node::Prim(p) => *cost_fn.cost_prim.get(p).unwrap_or(&cost_fn.cost_prim_default),
            Node::App(f, x) => {
                cost_fn.cost_app + self.get(*f).cost_rec(cost_fn) + self.get(*x).cost_rec(cost_fn)
            }
            Node::Lam(b) => {
                cost_fn.cost_lam + self.get(*b).cost_rec(cost_fn)
            }
        }
    }

    pub fn copy_span(&self, other_set: &mut ExprSet) -> Idx {
        let shift: i32 = other_set.iter().len() as i32 - self.get_span().unwrap().start as i32;
        // extend everything on while shfiting it
        other_set.nodes.extend(self.iter_span().map(|i| {
            let node = self.get_node(i);
            match node {
                Node::Prim(_) | Node::Var(_) | Node::IVar(_) => node.clone(),
                Node::App(f, x) => Node::App((*f as i32 + shift) as usize, (*x as i32 + shift) as usize),
                Node::Lam(b) => Node::Lam((*b as i32 + shift) as usize),
            }
        }));

        // shift all the spans and extend them on
        if let Some(other_spans) = &mut other_set.spans {
            other_spans.extend(self.iter_span().map(|i| {
                let span = self.get(i).get_span().unwrap();
                (span.start as i32 + shift) as usize .. (span.end as i32 + shift) as usize
            }))
        }

        // reverse order if we have opposite orders
        if self.set.order == Order::ChildFirst && other_set.order == Order::ParentFirst
            || self.set.order == Order::ParentFirst && other_set.order == Order::ChildFirst
        {
            let len = other_set.nodes.len();
            other_set.nodes[len - self.iter_span().len()..].reverse();
            if let Some(other_spans) = &mut other_set.spans {
                other_spans[len - self.iter_span().len()..].reverse();
            }
        }

        // ensure if we're Any then they are not Any
        if self.set.order == Order::Any && other_set.order != Order::Any {
            panic!("breaking order invariant")
        }

        (self.idx as i32 + shift) as usize
    }

    pub fn node_order_safe(&self) -> bool {
        match self.get(self.idx).node() {
            Node::Prim(_) | Node::Var(_) | Node::IVar(_) => true,
            Node::App(f, x) => match self.set.order {
                Order::ChildFirst => (*f == HOLE || *f < self.idx) && (*x == HOLE || *x < self.idx),
                Order::ParentFirst => (*f == HOLE || *f > self.idx) && (*x == HOLE || *x > self.idx),
                Order::Any => *f != self.idx && *x != self.idx,
            },
            Node::Lam(b) => match self.set.order {
                Order::ChildFirst => *b == HOLE || *b < self.idx,
                Order::ParentFirst => *b == HOLE || *b > self.idx,
                Order::Any => *b != self.idx,
            },
        }
    }
}

impl<'a> ExprMut<'a> {
    #[inline(always)]
    pub fn get(&mut self, idx: Idx) -> ExprMut {
        ExprMut { set: self.set, idx }
    }
    #[inline(always)]
    pub fn get_node(&'a self, idx: Idx) -> &'a Node {
        &self.set[idx]
    }
    #[inline(always)]
    pub fn get_node_mut(&'a mut self, idx: Idx) -> &'a mut Node {
        &mut self.set[idx]
    }
    #[inline(always)]
    pub fn node(&mut self) -> &mut Node {
        &mut self.set[self.idx]
    }
    #[inline(always)]
    pub fn immut(&'a self) -> Expr<'a> {
        // let ExprMut {set, idx} = self;
        Expr {set: self.set, idx: self.idx}
    }
    pub fn expand(&mut self, idx: Idx) {
        match self.node() {
            Node::App(x,y) => {
                if *x == HOLE {
                    *x = idx;
                } else {
                    assert_eq!(*y, HOLE, "invalid expand() on non-hole");
                    *y = idx;    
                }
            },
            Node::Lam(b) => {
                assert_eq!(*b, HOLE, "invalid expand() on non-hole");
                *b = idx
            }
            _ => panic!("invalid expand() on non-lam non-app: {:?}", self.node())
        }
        debug_assert!(self.immut().node_order_safe());
    }
    pub fn expand_right(&mut self, idx: Idx) {
        match self.node() {
            Node::App(_,y) => {
                assert_eq!(*y, HOLE, "invalid expand_right() on non-hole");
                *y = idx;
            },
            Node::Lam(b) => {
                assert_eq!(*b, HOLE, "invalid expand_right() on non-hole");
                *b = idx
            }
            _ => panic!("invalid expand_right() on non-lam non-app: {:?}", self.node())
        }
        debug_assert!(self.immut().node_order_safe());
    }

    pub fn unexpand(&mut self) {
        match self.node() {
            Node::App(x,y) => {
                if *y != HOLE {
                    *y = HOLE;
                } else {
                    *x = HOLE;    
                }
            },
            Node::Lam(b) => {
                *b = HOLE
            }
            _ => panic!("invalid unexpand() on non-lam non-app: {:?}", self.node())
        }
        debug_assert!(self.immut().node_order_safe());
    }

    pub fn unexpand_right(&mut self) {
        match self.node() {
            Node::App(_,y) => {
                if *y != HOLE {
                    *y = HOLE;
                }
            },
            Node::Lam(b) => {
                *b = HOLE
            }
            _ => panic!("invalid unexpand_right() on non-lam non-app: {:?}", self.node())
        }
        debug_assert!(self.immut().node_order_safe());
    }
}

// struct ExprIter<'a> {
//     curr: Expr<'a>,
//     iters: Vec<ExprIter<'a>>
// }

// impl<'a> Iterator for ExprIter<'a> {
//     type Item = Expr<'a>;

//     fn next(&mut self) -> Option<Self::Item> {
//         if !self.iters.is_empty() {
//             iters.first
//         }
//         match self.curr.node() {
//             Node::Var(_) => Some(self.curr),
//             Node::Prim(_) => Some(self.curr),
//             Node::App(f, x) => todo!(),
//             Node::Lam(b) => ExprIter { curr: Expr { self.curr.set,  } },
//             Node::IVar(_) => Some(self.curr),
//         }
//     }
// }


/// the cost of a program, where `app` and `lam` cost 1, `programs` costs nothing,
/// `ivar` and `var` and `prim` cost 100.
#[derive(Debug,Clone)]
pub struct ExprCost {
    cost_lam: i32,
    cost_app: i32,
    cost_var: i32,
    cost_ivar: i32,
    cost_prim: HashMap<Symbol,i32>,
    cost_prim_default: i32,
}

impl ExprCost {
    fn dreamcoder() -> ExprCost {
        ExprCost {
            cost_lam: 1,
            cost_app: 1,
            cost_var: 100,
            cost_ivar: 100,
            cost_prim: HashMap::new(),
            cost_prim_default: 100,
        }
    }
}

// impl std::str::FromStr for ExprSet {
//     type Err = String;
//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         // assume uncurried string
//         let mut set = ExprSet::empty(Order::ChildFirst, Spans::None);
//         set.parse_extend(s)?;
//         Ok(set)
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr_basics() {
        let set = &mut ExprSet::empty(Order::ChildFirst, true);
        
        let e1 = set.parse_extend("(lam $0)").unwrap();
        let e2 = set.parse_extend("(+ 4 4)").unwrap();

        // bottom up style addition of a node
        let e3 = set.add(Node::App(e1,e2));
        assert_eq!(set.get(e3).to_string(), "((lam $0) (+ 4 4))".to_string());

        // iterators tend to return Idxs instead of &'a references to avoid
        // lifetime woes and allow for mutation and reading interleaved as shown
        // here
        for i in set.get(e1).iter_span() {
            let bonus = set.len() as i32;
            match set.get_mut(i).node() {
                Node::Var(i) => {*i += bonus},
                _ => {}
            }
        }

        assert_eq!(set.get(e3).to_string(), "((lam $8) (+ 4 4))".to_string());

        // test copy_span
        let mut other_set = &mut ExprSet::empty(Order::ChildFirst, true);
        let e4 = other_set.parse_extend("(lam (lam $1))").unwrap();
        let e3_new = set.get(e3).copy_span(other_set);
        assert_eq!(set.get(e3).to_string(), other_set.get(e3_new).to_string());

        // top down grow (+ 2 (lam 3)) ie (app (app + 2) 3)
        // notice we use ParentFirst
        let e = &mut ExprSet::empty(Order::ParentFirst, false);

        // ??
        assert_eq!(e.hole().to_string(), "??");

        // (app (app + ??) (lam ??))
        let app1 = e.add(Node::App(HOLE,HOLE));
        let app2 = e.add(Node::App(HOLE,HOLE));
        let plus = e.add(Node::Prim("+".into()));
        let lam = e.add(Node::Lam(HOLE));
        e.get_mut(app1).expand(app2);
        e.get_mut(app2).expand(plus);
        e.get_mut(app1).expand(lam);

        assert_eq!(e.get(app1).to_string(), "(+ ?? (lam ??))");
        let len = e.len();

        // (app (app + 2) (lam ??))
        let two = e.add(Node::Prim("2".into()));
        e.get_mut(app2).expand(two);

        assert_eq!(e.get(app1).to_string(), "(+ 2 (lam ??))");

        // (app (app + 2) (lam 3))
        let three = e.add(Node::Prim("3".into()));
        e.get_mut(lam).expand(three);

        assert_eq!(e.get(app1).to_string(), "(+ 2 (lam 3))");

        // roll back two steps to (app (app + ??) (lam ??))
        e.truncate(len);
        e.get_mut(lam).unexpand();
        e.get_mut(app2).unexpand();

        assert_eq!(e.get(app1).to_string(), "(+ ?? (lam ??))");

        




    }
}
