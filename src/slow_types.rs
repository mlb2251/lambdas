use std::collections::VecDeque;
use serde::{Serialize, Deserialize};

use crate::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SlowType {
    Var(usize), // type variable like t0 t1 etc
    Term(Symbol, Vec<SlowType>), // symbol is the name like "int" or "list" or "->" and Vec<Type> is the args which is empty list for things like int etc
}


impl SlowType {
    pub fn base(name: Symbol) -> SlowType {
        SlowType::Term(name, vec![])
    }

    pub fn arrow(left: SlowType, right: SlowType) -> SlowType {
        SlowType::Term(ARROW_SYM.clone(), vec![left, right])
    }

    pub fn is_arrow(&self) -> bool {
        match self {
            SlowType::Var(_) => false,
            SlowType::Term(name, _) => *name == *ARROW_SYM,
        }
    }

    pub fn as_arrow(&self) -> Option<(&SlowType, &SlowType)> {
        match self {
            SlowType::Term(name,args) => {
                if *name != *ARROW_SYM {
                    return None
                }
                assert_eq!(args.len(),2);
                Some((&args[0], &args[1]))
            },
            _ => None
        }
    }

    /// iterates over all nodes in the term of this type
    pub fn iter_arrows(&self) -> ArrowIter {
        ArrowIter { curr: self }
    }

    /// iterates over uncurried argument types of this arrow type
    pub fn iter_args(&self) -> impl Iterator<Item=&SlowType> {
        self.iter_arrows().map(|(left,_right)| left)
    }

    /// arity of this arrow type (zero if not an arrow type)
    pub fn arity(&self) -> usize {
        self.iter_args().count()
    }

    /// return type of this arrow types *after* uncurrying. For a non arrow type
    /// this just returns the type itself.
    pub fn return_type(&self) -> &SlowType {
        self.iter_arrows().last().map(|(_left,right)| right).unwrap_or(self)
    }

    /// true if there are no type vars in this type
    pub fn is_concrete(&self) -> bool {
        match self {
            SlowType::Var(_) => false,
            SlowType::Term(_, args) => args.iter().all(|ty| ty.is_concrete())
        }
    }

    /// true if type var i occurs in this type
    pub fn occurs(&self, i: usize) -> bool {
        match self {
            SlowType::Var(j)  => i == *j,
            SlowType::Term(_, args) => args.iter().any(|ty| ty.occurs(i))
        }
    }

    pub fn apply_cached(&self, ctx: &mut Context) -> SlowType {
        if self.is_concrete() {
            return self.clone();
        }
        match self {
            SlowType::Var(i) => {
                // look up the type var in the ctx to see if its bound
                if let Some(tp) = ctx.get(*i).cloned() {
                    // in case it's bound to something that ALSO has variables, we want to track those down too
                    let tp_applied = tp.apply(ctx);
                    if tp != tp_applied {
                        // and to save our work for the future, lets amortize it (union-find style) by saving what we
                        // found things were bound to. Since bindings will never change this is okay.
                        ctx.set(*i, tp_applied.clone())
                    }
                    tp_applied
                } else {
                    self.clone() // t0 is not bound by ctx so we leave it unbound
                }
            },
            SlowType::Term(name, args) => SlowType::Term(name.clone(), args.iter().map(|ty| ty.apply_cached(ctx)).collect())
        }
    }

    /// same as apply_cached() but doesnt do the unionfind style caching of results, so there's no need to mutate the ctx
    pub fn apply(&self, ctx: &Context) -> SlowType {
        if self.is_concrete() {
            return self.clone();
        }
        match self {
            SlowType::Var(i) => {
                // look up the type var in the ctx to see if its bound
                if let Some(tp) = ctx.get(*i).cloned() {
                    // in case it's bound to something that ALSO has variables, we want to track those down too
                    tp.apply(ctx)
                } else {
                    self.clone() // t0 is not bound by ctx so we leave it unbound
                }
            },
            SlowType::Term(name, args) => SlowType::Term(name.clone(), args.iter().map(|ty| ty.apply(ctx)).collect())
        }
    }


    /// shifts all variables in a type such that they are fresh variables in the context, returning a new type
    pub fn instantiate(&self, ctx: &mut Context) -> SlowType {
        if self.is_concrete() {
            return self.clone()
        }
        fn instantiate_aux(ty: &SlowType, ctx: &mut Context, shift_by: usize) -> SlowType {
            match ty {
                SlowType::Var(i) => {
                    let new = i + shift_by;
                    ctx.fresh_type_vars(new);
                    assert!(ctx.get(new).is_none());
                    SlowType::Var(new)
                },
                SlowType::Term(name, args) => SlowType::Term(name.clone(), args.iter().map(|t| instantiate_aux(t, ctx, shift_by)).collect()),
            }
        }
        // shift by the highest var that already exists, so that theres no conflict
        instantiate_aux(self, ctx, ctx.next_var)
    }
}

pub struct ArrowIter<'a> {
    curr: &'a SlowType
}

impl<'a> Iterator for ArrowIter<'a> {
    type Item = (&'a SlowType, &'a SlowType);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((left,right)) = self.curr.as_arrow() {
            self.curr = right;
            Some((left,right))
        } else {
            None
        }
    }
}


impl std::str::FromStr for SlowType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_type::parse(s)
    }
}

impl std::fmt::Display for SlowType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn helper(ty: &SlowType, f: &mut std::fmt::Formatter<'_>, arrow_parens: bool) -> std::fmt::Result {
            match ty {
                SlowType::Var(i) => write!(f,"t{i}"),
                SlowType::Term(name, args) => {
                    if args.is_empty() {
                        write!(f, "{name}")
                    } else if *name == *ARROW_SYM {
                        assert_eq!(args.len(), 2);
                        // write!(f, "({} {} {})", &args[0], name, &args[1])
                        if arrow_parens {
                            write!(f, "(")?;
                        }
                        helper(&args[0], f, true)?;
                        write!(f, " {} ", ARROW_SYM.as_ref())?;
                        helper(&args[1], f, false)?;
                        if arrow_parens {
                            write!(f, ")")?;
                        }
                        Ok(())
                    } else {
                        write!(f, "({name}")?;
                        for arg in args.iter() {
                            write!(f, " ")?;
                            helper(arg, f, true)?;
                        }
                        write!(f, ")")
                    }
                },
            }
        }
        helper(self, f, true)
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    subst_unionfind: Vec<Option<SlowType>>, // todo also try ahashmap tho i just wanted to avoid the allocations
    subst_append_only: Vec<(usize,SlowType)>,
    next_var: usize,
    append_only: bool,
}

impl Context {

    /// This is the usual way of creating a new Context. The context will be append-only
    /// meaning you can roll it back to a point by truncating
    pub fn empty() -> Context {
        Context {
            subst_unionfind: Default::default(),
            subst_append_only: Default::default(),
            next_var: 0,
            append_only: true,
        }
    }

    /// instead of an append-only substitution, the context will instead use a unionfind. This is honestly
    /// likely not noticably faster and doesnt allow rollbacks. It may even be slower.
    pub fn empty_unionfind() -> Context {
        Context {
            subst_unionfind: Default::default(),
            subst_append_only: Default::default(),
            next_var: 0,
            append_only: false,
        }
    }

    pub fn save_state(&self) -> (usize,usize) {
        assert!(self.append_only);
        (self.subst_append_only.len(), self.next_var)
    }

    pub fn load_state(&mut self, state: (usize,usize)) {
        assert!(self.append_only);
        self.subst_append_only.truncate(state.0);
        self.next_var = state.1;
    }

    fn fresh_type_var(&mut self) -> SlowType {
        if !self.append_only {
            self.subst_unionfind.push(None);
        }
        self.next_var += 1;
        SlowType::Var(self.next_var-1)
    }

    /// adds new fresh type vars as necessary such that variable Var exists
    #[inline(always)]
    fn fresh_type_vars(&mut self, var: usize) {
        while var >= self.next_var {
            self.fresh_type_var();
        }
    }

    /// a very quick non-allocating check that returns false if it's
    /// obvious that these types won't unify. This works *even when a type hasnt
    /// been instantiated() to have new type variables*. First this checks if t1 and t2 have the same constructors
    /// and if theres an obvious mismatch there it gives up. Then it goes and looks up the types in the ctx
    /// in case they were typevars, and then again checks if they have th same constructor. It uses apply_immut() to
    /// avoid mutating the context for this lookup.
    /// Note the apply_immut version of this was wrong bc thats only safe to do on the hole_tp side and apply_immut
    /// is already done to the hole before then anyways
    pub fn might_unify(t1: &SlowType, t2: &SlowType) -> bool {
        match (t1,t2) {
            (SlowType::Var(_), SlowType::Var(_)) => true,
            (SlowType::Var(_), SlowType::Term(_, _)) => true,
            (SlowType::Term(_, _), SlowType::Var(_)) => true,
            (SlowType::Term(x, xs), SlowType::Term(y, ys)) => {
                x == y && xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x,y)| Context::might_unify(x,y))
            },
        }
    }

    /// Normal unification. Does not do the amortizing step of the unionfind (but may mutate
    /// it still). See unify_cached() for amortized unionfind. Note that this is likely not slower
    /// than unify_cached() in most cases.
    pub fn unify(&mut self, t1: &SlowType,  t2: &SlowType) -> UnifyResult {
        // println!("\tunify({},{}) {}", t1, t2, self);
        let t1: SlowType = t1.apply(self);
        let t2: SlowType = t2.apply(self);
        // println!("\t  ...({},{}) {}", t1, t2, self);
        if t1.is_concrete() && t2.is_concrete() {
            // if both types are concrete, simple equality works because we dont need to do any fancy variable binding
            if t1 == t2 {
                return Ok(())
            } else {
                return Err(UnifyErr::ConcreteSubtree)
            }
        }
        match (t1, t2) {
            (SlowType::Var(i), ty) | (ty, SlowType::Var(i)) => {
                if ty == SlowType::Var(i) { return Ok(()) } // unify(t0, t0) -> true
                if ty.occurs(i) { return Err(UnifyErr::Occurs) } // recursive type  e.g. unify(t0, (t0 -> int)) -> false
                // *** Above is the "occurs" check, which prevents recursive definitions of types. Removing it would allow them.

                assert!(self.get(i).is_none());
                self.set(i, ty);
                Ok(())
            },
            (SlowType::Term(x, xs), SlowType::Term(y, ys)) => {
                // simply recurse
                if x != y || xs.len() != ys.len() {
                    return Err(UnifyErr::Production)
                }
                xs.iter().zip(ys.iter()).try_for_each(|(x,y)| self.unify(x,y))
            }
        }
    }

    /// [expert mode] like unify() but uses apply_cached() to do amortization step of
    /// unionfind. Likely not worth using compared to unify().
    pub fn unify_cached(&mut self, t1: &SlowType,  t2: &SlowType) -> UnifyResult {
        // println!("unify({},{}) {}", t1, t2, self);
        let t1: SlowType = t1.apply_cached(self);
        let t2: SlowType = t2.apply_cached(self);
        // println!("  ...({},{}) {}", t1, t2, self);
        if t1.is_concrete() && t2.is_concrete() {
            // if both types are concrete, simple equality works because we dont need to do any fancy variable binding
            if t1 == t2 {
                return Ok(())
            } else {
                return Err(UnifyErr::ConcreteSubtree)
            }
        }
        match (t1, t2) {
            (SlowType::Var(i), ty) | (ty, SlowType::Var(i)) => {
                if ty == SlowType::Var(i) { return Ok(()) } // unify(t0, t0) -> true
                if ty.occurs(i) { return Err(UnifyErr::Occurs) } // recursive type  e.g. unify(t0, (t0 -> int)) -> false
                // *** Above is the "occurs" check, which prevents recursive definitions of types. Removing it would allow them.

                assert!(self.subst_unionfind.get(i).is_none());
                self.set(i, ty);
                Ok(())
            },
            (SlowType::Term(x, xs), SlowType::Term(y, ys)) => {
                // simply recurse
                if x != y || xs.len() != ys.len() {
                    return Err(UnifyErr::Production)
                }
                xs.iter().zip(ys.iter()).try_for_each(|(x,y)| self.unify(x,y))
            }
        }
    }

    /// get what a variable is bound to (if anything).
    #[inline(always)]
    fn get(&self, var: usize) -> Option<&SlowType> { // todo written in a silly way, rewrite
        if self.append_only {
            self.subst_append_only.iter().rfind(|(i,_)| *i == var).map(|(_,tp)| tp)
        } else {
            self.subst_unionfind[var].as_ref()
        }
    }
    /// set what a variable is bound to
    #[inline(always)]
    fn set(&mut self, var: usize, ty: SlowType) {
        if self.append_only {
            self.subst_append_only.push((var,ty));
        } else {
            self.subst_unionfind[var] = Some(ty);
        }
    }

}

impl std::fmt::Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{{")?;
        let mut first: bool = true;
        for (i, item) in self.subst_unionfind.iter().enumerate() {
            if let Some(ty) = item {
                if !first { write!(f, ", ")? } else { first = false }
                write!(f, "{i}:{ty}")?
            }
        }
        write!(f,"}}")
    }
}


impl<'a> Expr<'a> {
    pub fn infer<D: Domain>(&self, ctx: &mut Context, env: &mut VecDeque<SlowType>, dsl: &DSL<D>) -> Result<SlowType,UnifyErr> {
        // println!("infer({})", self.to_string_uncurried(child));
        match self.node() {
            Node::App(f,x) => {
                let return_tp = ctx.fresh_type_var();
                let x_tp = self.get(*x).infer::<D>(ctx, env, dsl)?;
                let f_tp = self.get(*f).infer::<D>(ctx, env, dsl)?;
                ctx.unify(&f_tp, &SlowType::arrow(x_tp, return_tp.clone()))?;
                Ok(return_tp.apply(ctx))
            },
            Node::Lam(b, _, _) => {
                // TOOD add arity to types
                let var_tp = ctx.fresh_type_var();
                // todo maybe optimize by making this a vecdeque for faster insert/remove at the zero index
                env.push_front(var_tp.clone());
                let body_tp = self.get(*b).infer::<D>(ctx, env, dsl)?;
                env.pop_front();
                Ok(SlowType::arrow(var_tp, body_tp).apply(ctx))
            },
            Node::Var(i, _) => {
                if (*i as usize) >= env.len() {
                    panic!("unbound variable encountered during infer(): ${}", i)
                }
                Ok(env[*i as usize].apply(ctx))
            },
            Node::IVar(_i) => {
                // interesting, I guess we can have this and it'd probably be easy to do
                unimplemented!();
            }
            Node::Prim(p) => {
                Ok(dsl.type_of_prim(p).instantiate(ctx))
            },
        }
    }
    // pub fn infer_ref<D: Domain>(&self, ctx: &mut TypeSet, env: &mut VecDeque<TypeRef>) -> Result<TypeRef,UnifyErr> {
    //     // println!("infer({})", self.to_string_uncurried(child));
    //     match self.node() {
    //         Node::App(f,x) => {
    //             let return_tp = ctx.fresh_type_var();
    //             let x_tp = self.get(*x).infer_ref::<D>(ctx, env)?;
    //             let f_tp = self.get(*f).infer_ref::<D>(ctx, env)?;
    //             let arrow_tp = ctx.add_arrow(f_tp, x_tp);
    //             ctx.unify(&f_tp, &Type::arrow(x_tp, return_tp.clone()))?;
    //             Ok(return_tp.apply(ctx))
    //         },
    //         Node::Lam(b) => {
    //             let var_tp = ctx.fresh_type_var();
    //             // todo maybe optimize by making this a vecdeque for faster insert/remove at the zero index
    //             env.push_front(var_tp.clone());
    //             let body_tp = self.get(*b).infer_ref::<D>(ctx, env)?;
    //             env.pop_front();
    //             Ok(Type::arrow(var_tp, body_tp).apply(ctx))
    //         },
    //         Node::Var(i) => {
    //             if (*i as usize) >= env.len() {
    //                 panic!("unbound variable encountered during infer(): ${}", i)
    //             }
    //             Ok(env[*i as usize].apply(ctx))
    //         },
    //         Node::IVar(_i) => {
    //             // interesting, I guess we can have this and it'd probably be easy to do
    //             unimplemented!();
    //         }
    //         Node::Prim(p) => {
    //             Ok(D::type_of_prim(p).instantiate(ctx))
    //         },
    //     }
    // }
    
}


#[test]
fn test_parse_types() {
    assert_eq!("int".parse::<SlowType>().unwrap(),
        SlowType::Term("int".into(), vec![]));

    assert_eq!("((int))".parse::<SlowType>().unwrap(),
        SlowType::Term("int".into(), vec![]));

    assert_eq!("list int".parse::<SlowType>().unwrap(),
    SlowType::Term("list".into(), vec![
        SlowType::Term("int".into(), vec![])
    ]));

    assert_eq!("(foo -> bar)".parse::<SlowType>().unwrap(),
    SlowType::Term(ARROW_SYM.clone(), vec![
        SlowType::Term("foo".into(), vec![]),
        SlowType::Term("bar".into(), vec![]),
    ]));

    assert_eq!("foo -> bar".parse::<SlowType>().unwrap(),
    SlowType::Term(ARROW_SYM.clone(), vec![
        SlowType::Term("foo".into(), vec![]),
        SlowType::Term("bar".into(), vec![]),
    ]));

    assert_eq!("(foo -> bar -> baz)".parse::<SlowType>().unwrap(),
    SlowType::Term(ARROW_SYM.clone(), vec![
        SlowType::Term("foo".into(), vec![]),
        SlowType::Term(ARROW_SYM.clone(), vec![
            SlowType::Term("bar".into(), vec![]),
            SlowType::Term("baz".into(), vec![]),
        ]),
    ]));

    assert_eq!("t2".parse::<SlowType>().unwrap(),
        SlowType::Var(2));


    // the map() type
    assert_eq!("(t0 -> t1) -> (list t0) -> (list t1)".parse::<SlowType>().unwrap(),
    SlowType::Term(ARROW_SYM.clone(), vec![
        SlowType::Term(ARROW_SYM.clone(), vec![
            SlowType::Var(0),
            SlowType::Var(1),
        ]),
        SlowType::Term(ARROW_SYM.clone(), vec![
            SlowType::Term("list".into(), vec![
                SlowType::Var(0)
            ]),
            SlowType::Term("list".into(), vec![
                SlowType::Var(1)
            ]),
        ]),
    ]));


    // test load_types
    // load_types(Path::new("data/types_origami.json"));
    
}

