
use crate::*;
use once_cell::sync::Lazy;

pub static ARROW_SYM: Lazy<Symbol> = Lazy::new(|| Symbol::from("->"));


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnifyErr {
    Occurs,
    ConcreteSubtree,
    Production
}
pub type UnifyResult = Result<(), UnifyErr>;



/// TNodes are members of TypeSets
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TNode {
    Var(usize), // type variable like t0 t1 etc; index is used to index into a Context not the TypeSet.
    Arrow(Args,Idx), // arrow type
    Term(Symbol, Args),
}

/// an unrolled representation of arguments to decrease on pointer chases and Vec-based heap allocation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Args {
    Args0,
    Args1(Idx),
    Args2(Idx, Idx),
    Args3(Idx, Idx, Idx),
    ArgsN(Vec<Idx>),
}

impl Args {

    // get the arity
    pub fn len(&self) -> usize {
        match self {
            Args::Args0 => 0,
            Args::Args1(_) => 1,
            Args::Args2(_, _) => 2,
            Args::Args3(_, _, _) => 3,
            Args::ArgsN(args) => args.len(),
        }
    }

    /// get the ith argument
    pub fn get(&self, i: usize) -> Option<Idx> {
        match self {
            Args::Args0 => None,
            Args::Args1(a) => if i == 0 { Some(*a) } else { None },
            Args::Args2(a, b) => match i {
                0 => Some(*a),
                1 => Some(*b),
                _ => None,
            },
            Args::Args3(a, b, c) => match i {
                0 => Some(*a),
                1 => Some(*b),
                2 => Some(*c),
                _ => None,
            },
            Args::ArgsN(args) => args.get(i).copied(),
        }
    }

    /// iterate over the arguments
    pub fn iter(&self) -> impl ExactSizeIterator<Item=Idx> + '_ {
        (0..self.len()).map(move |i| self.get(i).unwrap())
    }
        
}

impl From<Vec<Idx>> for Args {
    fn from(args: Vec<Idx>) -> Self {
        match args.len() {
            0 => Args::Args0,
            1 => Args::Args1(args[0]),
            2 => Args::Args2(args[0], args[1]),
            3 => Args::Args3(args[0], args[1], args[2]),
            _ => Args::ArgsN(args),
        }
    }
}



/// An index into a typeset, which implies shifting any Var at the idx by `shift`.
/// If set[idx] is a Var then shift it and lookup what it points to in the Context
/// If set[idx] is a Term then shift all of its args by `shift`
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type {
    pub idx: Idx,
    pub shift: usize,
}

impl Type {
    #[inline(always)]
    pub fn new(idx: Idx, shift: usize) -> Self {
        Self { idx, shift }
    }
}

// #[derive(Debug, Clone)]
// pub struct TypeRef<'a> {
//     tp: TypeIdx,
//     set: &'a TypeSet,
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSet {
    pub nodes: Vec<TNode>,
    pub max_var: Vec<Option<usize>>,
    pub subst: Vec<(usize,Type)>,
    pub next_var: usize,
}

impl TypeSet {

    /// This is the usual way of creating a new Context. The context will be append-only
    /// meaning you can roll it back to a point by truncating
    pub fn empty() -> TypeSet {
        TypeSet {
            nodes: Default::default(),
            max_var: Default::default(),
            subst: Default::default(),
            next_var: 0,
        }
    }

    pub fn save_state(&self) -> (usize,usize) {
        (self.subst.len(), self.next_var)
    }

    pub fn load_state(&mut self, state: (usize,usize)) {
        self.subst.truncate(state.0);
        self.next_var = state.1;
    }

    /// Instantiate an index into a shifted type using all fresh type variables
    /// and making the shift high enough
    #[inline(never)]
    pub fn instantiate(&mut self, idx: Idx) -> Type {
        let shift = self.next_var;
        if let Some(max_var) = self.max_var[idx] {
            // create a fresh type var for each new variable
            for _ in 0..=max_var {
                self.fresh_type_var();
            }
        }
        Type { idx, shift }
    }

    pub fn add_tp(&mut self, tp: &SlowType) -> Idx {
        match tp {
            SlowType::Var(i) => {
                self.add_node(TNode::Var(*i))
            }
            SlowType::Term(p, args) => {
                if tp.is_arrow() {
                    let return_tp = self.add_tp(tp.return_type());
                    let arrow_args: Vec<Idx> = tp.iter_args().map(|arg| self.add_tp(arg)).collect();
                    self.add_node(TNode::Arrow(Args::from(arrow_args), return_tp))
                } else {
                    let args: Vec<Idx> = args.iter().map(|arg| self.add_tp(arg)).collect();
                    self.add_node(TNode::Term(p.clone(), Args::from(args)))
                }
            },
        }
    }
    #[inline(always)]
    pub fn add_node(&mut self, node: TNode) -> Idx {
        let max_var = match &node {
            TNode::Var(i) => Some(*i),
            TNode::Term(_, args) | TNode::Arrow(args, _) =>
                args.iter().filter_map(|arg| self.max_var[arg]).max(),
        };
        self.max_var.push(max_var);
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    /// Normal unification. Does not do the amortizing step of the unionfind (but may mutate
    /// it still). See unify_cached() for amortized unionfind. Note that this is likely not slower
    /// than unify_cached() in most cases.
    #[inline(never)]
    pub fn unify(&mut self, t1: &Type,  t2: &Type) -> UnifyResult {
        let (node1,t1) = t1.node(self);
        let (node2,t2) = t2.node(self);

        match (node1,node2) {
            (TNode::Arrow(_, _), TNode::Term(_, _)) | (TNode::Term(_, _), TNode::Arrow(_, _)) => {
                return Err(UnifyErr::Production)
            }
            (TNode::Arrow(args1, _), TNode::Arrow(args2, _)) => {
                if args1.len() != args2.len() {
                    return Err(UnifyErr::Production)
                }
            }
            (TNode::Term(p1, args1), TNode::Term(p2, args2)) => {
                if p1 != p2 || args1.len() != args2.len() {
                    return Err(UnifyErr::Production)
                }
            }
            _ => {}
        }


        match (node1.clone(),node2.clone()) {
            (TNode::Var(i1), _) => {
                let i1_shifted = i1 + t1.shift;
                // check for identical variable (only needs to happen on this match case bc later one cant have a Var for both)
                if let TNode::Var(i2) = node2 {
                    if i1_shifted == i2 + t2.shift {
                        return Ok(()); // unify(t0, t0) -> true
                    }
                }
                // *** "occurs" check, which prevents recursive definitions of types. Removing it would allow them.
                if t2.occurs(i1_shifted, self) { return Err(UnifyErr::Occurs) } // recursive type  e.g. unify(t0, (t0 -> int)) -> false

                // set the varisble
                debug_assert!(self.get_var(i1_shifted).is_none());
                self.set_var(i1_shifted, t2);
                Ok(())
            }
            (_, TNode::Var(i2)) => {
                let i2_shifted = i2 + t2.shift;
                // *** "occurs" check, which prevents recursive definitions of types. Removing it would allow them.
                if t1.occurs(i2_shifted, self) { return Err(UnifyErr::Occurs) } // recursive type  e.g. unify(t0, (t0 -> int)) -> false

                // set the varisble
                debug_assert!(self.get_var(i2_shifted).is_none());
                self.set_var(i2_shifted, t1);
                Ok(())
            }

            (TNode::Term(x, xs), TNode::Term(y, ys)) =>
            {
                // simply recurse
                if x != y || xs.len() != ys.len() {
                    return Err(UnifyErr::Production)
                }

                for (x,y) in xs.iter().zip(ys.iter()) {
                    self.unify(&Type::new(x,t1.shift),&Type::new(y,t2.shift))?;
                }
                Ok(())
            }
            (TNode::Arrow(xargs, xret), TNode::Arrow(yargs, yret)) =>
            {
                // simply recurse
                if xargs.len() != yargs.len() {
                    return Err(UnifyErr::Production)
                }

                for (x,y) in xargs.clone().iter().zip(yargs.clone().iter()) {
                    self.unify(&Type::new(x,t1.shift),&Type::new(y,t2.shift))?;
                }

                self.unify(&Type::new(xret,t1.shift),&Type::new(yret,t2.shift))?;
                Ok(())
            }
            (TNode::Arrow(_, _), TNode::Term(_, _)) | (TNode::Term(_, _), TNode::Arrow(_, _)) => {
                Err(UnifyErr::Production)
            }
        }
    }

    /// Make a fresh type variable
    #[inline(always)]
    fn fresh_type_var(&mut self) -> TNode {
        self.next_var += 1;
        TNode::Var(self.next_var - 1)
    }

    /// get what a variable is bound to (if anything).
    #[inline(always)]
    fn get_var(&self, var: usize) -> Option<&Type> {
        self.subst.iter().rfind(|(i,_)| *i == var).map(|(_,tp)| tp)
    }
    /// set what a variable is bound to
    #[inline(always)]
    fn set_var(&mut self, var: usize, ty: Type) {
        self.subst.push((var,ty));
    }



}



impl Type {

    /// get our node and shift.
    /// - If we are not a Var we just return our own node and shift.
    /// - If we are a Var we lookup what it points to in the subst (when our shift is added) and return that, not applying our own shift.
    #[inline(always)]
    pub fn node<'a>(&self, set: &'a TypeSet) -> (&'a TNode, Type) {
        let tp = self.canonicalize(set);
        (&set.nodes[tp.idx], tp)
    }

    /// You should never really need to call this because node() calls it for you
    #[inline(always)]
    fn canonicalize(&self, set: &TypeSet) -> Type {
        let mut ret = *self;
        loop {
            if let TNode::Var(i) = &set.nodes[ret.idx] {
                if let Some(tp) = set.get_var(*i + ret.shift) {
                    ret = tp.canonicalize(set) // recursively resolve the lookup result using the shift of the lookup result itself
                } else {
                    return ret
                }
            } else {
                return ret
            }
        }
    }

    /// true if type var i occurs in this type (post-shifting of this type).
    pub fn occurs(&self, i: usize, set: &TypeSet) -> bool {
        let (node, tp) = self.node(set);
        match node {
            TNode::Var(j)  => i == j + tp.shift,
            TNode::Term(_, args) => {
                args.iter().any(|arg| Type::new(arg,tp.shift).occurs(i, set))
            },
            TNode::Arrow(args, ret_tp) => {
                args.iter().any(|arg| Type::new(arg,tp.shift).occurs(i, set))
                || Type::new(*ret_tp,tp.shift).occurs(i, set)
            },
        }
    }

    pub fn return_type(&self, set: &TypeSet) -> Type {
        let (node, tp) = self.node(set);
        match node {
            TNode::Arrow(_, ret_tp) => Type::new(*ret_tp, tp.shift),
            _ => *self,
        }
    }

    pub fn iter_args<'a>(&self, set: &'a TypeSet) -> impl ExactSizeIterator<Item=Type> + 'a {
        let (node, tp) = self.node(set);
        match node {
            TNode::Arrow(args, _) => args.iter().map(move |arg| Type::new(arg, tp.shift)),
            _ => panic!("not an arrow"),
        }
    }

    pub fn arity(&self, set: &TypeSet) -> usize {
        let (node, _) = self.node(set);
        match node {
            TNode::Arrow(args, _) => args.len(),
            _ => 0,
        }
    }

    pub fn is_arrow(&self, set: &TypeSet) -> bool {
        let (node, _) = self.node(set);
        match node {
            TNode::Arrow(_, _) => true,
            _ => false,
        }
    }
}





// impl RawTypeRef {

    // /// convenience method for converting to types. probably super slow but useful for debugging
    // #[inline(never)]
    // pub fn tp(&self, typeset: &TypeSet) -> SlowType {
    //     match self.node(typeset) {
    //         TNode::Var(i) => SlowType::Var(*i),
    //         TNode::Term(p, args) => {
    //             SlowType::Term(p.clone(), args.iter().map(|arg| arg.tp(typeset)).collect())
    //         },
    //     }
    // }

    // pub fn show(&self, typeset: &TypeSet) -> String {
    //     self.tp(typeset).to_string()
    // }

    // pub fn as_arrow(&self, typeset: &TypeSet) -> Option<(RawTypeRef, RawTypeRef)> {
    //     if let TNode::Term(name,args) = self.node(typeset) {
    //         if *name != *ARROW_SYM {
    //             return None
    //         }
    //         let mut it = args.iter();
    //         let left = it.next().unwrap();
    //         let right = it.next().unwrap();
    //         assert!(it.next().is_none(), "malformed arrow");
    //         Some((*left,*right))
    //     } else {
    //          None
    //     }
    // }

    // pub fn is_arrow(&self, typeset: &TypeSet) -> bool {
    //     if let TNode::Term(name,_) = self.node(typeset) {
    //         return *name == *ARROW_SYM
    //     }
    //     false
    // }

    // /// iterates over all nodes in the term of this type
    // pub fn iter_arrows<'a>(&self, typeset: &'a TypeSet) -> ArrowIterTypeRef<'a> {
    //     ArrowIterTypeRef { curr: *self, typeset }
    // }

    // /// iterates over uncurried argument types of this arrow type
    // pub fn iter_args<'a>(&self, typeset: &'a TypeSet) -> impl Iterator<Item=RawTypeRef> + 'a {
    //     self.iter_arrows(typeset).map(|(left,_right)| left)
    // }

    // /// arity of this arrow type (zero if not an arrow type)
    // pub fn arity(&self, typeset: &TypeSet) -> usize {
    //     self.iter_args(typeset).count()
    // }

    // /// return type of this arrow types *after* uncurrying. For a non arrow type
    // /// this just returns the type itself.
    // pub fn return_type(&self, typeset: &TypeSet) -> RawTypeRef {
    //     self.iter_arrows(typeset).last().map(|(_left,right)| right).unwrap_or(*self)
    // }

    // /// true if there are no type vars in this type
    // pub fn is_concrete(&self, typeset: &TypeSet) -> bool {
    //     match self.node(typeset) {
    //         TNode::Var(_) => false,
    //         TNode::Term(_,args) => args.iter().all(|ty| ty.is_concrete(typeset)),
    //     }
    // }

// }


// impl Type {


    // pub fn tp(&self, typeset: &TypeSet) -> SlowType {
    //     self.raw.tp(typeset)
    // }

    // pub fn show(&self, typeset: &TypeSet) -> String {
    //     format!("[shift {}] {}", self.shift, self.raw.tp(typeset))
    // }

//     pub fn as_arrow(&self, typeset: &TypeSet) -> Option<(Type, Type)> {
//         let canonical = self.canonicalize(typeset);
//         canonical.raw.as_arrow(typeset).map(|(r1,r2)| (r1.shift(canonical.shift),r2.shift(canonical.shift)))
//     }

//     pub fn is_arrow(&self, typeset: &TypeSet) -> bool {
//         if let TNode::Term(name,_) = self.canonicalize(typeset).raw.node(typeset) {
//             return *name == *ARROW_SYM
//         }
//         false
//     }

//     /// iterates over all nodes in the term of this type
//     pub fn iter_arrows<'a>(&'a self, typeset: &'a TypeSet) -> impl Iterator<Item=(Type,Type)> + 'a {
//         let canonical = self.canonicalize(typeset);
//         canonical.raw.iter_arrows(typeset).map(move |(r1,r2)| (r1.shift(canonical.shift),r2.shift(canonical.shift)))
//     }

//     /// iterates over uncurried argument types of this arrow type
//     pub fn iter_args<'a>(&'a self, typeset: &'a TypeSet) -> impl Iterator<Item=Type> + 'a {
//         self.iter_arrows(typeset).map(|(left,_right)| left)
//     }

//     /// arity of this arrow type (zero if not an arrow type)
//     pub fn arity(&self, typeset: &TypeSet) -> usize {
//         self.iter_args(typeset).count()
//     }

//     /// return type of this arrow types *after* uncurrying. For a non arrow type
//     /// this just returns the type itself.
//     pub fn return_type(&self, typeset: &TypeSet) -> Type {
//         self.iter_arrows(typeset).last().map(|(_left,right)| right).unwrap_or(*self)
//     }

//     /// true if there are no type vars in this type
//     pub fn is_concrete(&self, typeset: &TypeSet) -> bool {
//         let canonical = self.canonicalize(typeset);
//         match canonical.raw.node(typeset) {
//             TNode::Var(_) => false,
//             TNode::Term(_, args) => args.iter().map(|r|r.shift(canonical.shift)).all(|ty| ty.is_concrete(typeset)),
//         }
//     }

// }
