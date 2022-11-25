use crate::*;

use std::collections::HashMap;
use std::fmt::{Debug};
use std::hash::Hash;


pub type DSLFn<D> = fn(Env<D>, &Evaluator<D>) -> VResult<D>;

#[derive(Clone)]
pub struct Production<D: Domain> {
    pub name: Symbol, // eg "map" or "0" or "[1,2,3]"
    pub val: Val<D>,
    pub tp: Type,
    pub arity: usize,
    pub fn_ptr: Option<DSLFn<D>>,
}

impl<D:Domain> Debug for Production<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Production").field("name", &self.name).field("val", &self.val).field("tp", &self.tp).field("arity", &self.arity).finish()
    }
}


#[derive(Clone, Debug)]
pub struct DSL<D:Domain> {
    pub productions: HashMap<Symbol,Production<D>>,
    // pub lookup_fn_ptr: HashMap<Symbol,DSLFn<D>>,
}

impl<D: Domain> Production<D> {

    pub fn val(name: &str, tp: &str, val: Val<D>) -> Self {
        Production::val_raw(name.into(), tp.parse().unwrap(), val)
    }

    pub fn func(name: &str, tp: &str, fn_ptr: DSLFn<D>) -> Self {
        Production::func_raw(name.into(), tp.parse().unwrap(), fn_ptr)
    }

    pub fn val_raw(name: Symbol, tp: Type, val: Val<D>) -> Self {
        assert_eq!(tp.arity(),0);
        Production {
            name,
            val,
            tp,
            arity: 0,
            fn_ptr: None,
        }
    }

    pub fn func_raw(name: Symbol, tp: Type, fn_ptr: DSLFn<D>) -> Self {
        let arity = tp.arity();
        Production {
            name: name.clone(),
            val: PrimFun(CurriedFn::<D>::new(name, arity)),
            tp,
            arity,
            fn_ptr: Some(fn_ptr),
        }
    }



}
impl<D: Domain> DSL<D> {
    pub fn new(productions: Vec<Production<D>>) -> Self {
        DSL {
            productions: productions.into_iter().map(|entry| (entry.name.clone(), entry)).collect(),
        }
    }

    /// add an entry to the DSL
    pub fn add_entry(&mut self, entry: Production<D>) {
        assert!(!self.productions.contains_key(&entry.name));
        self.productions.insert(entry.name.clone(), entry);
    }

    /// given a primitive's symbol return a runtime Val object. For function primitives
    /// this should return a PrimFun(CurriedFn) object.
    pub fn val_of_prim(&self, p: &Symbol) -> Option<Val<D>> {
        self.productions.get(p).map(|entry| entry.val.clone()).or_else(||
            D::val_of_prim_fallback(p))
    }

    pub fn type_of_prim(&self, p: &Symbol) -> Type {
        self.productions.get(p).map(|entry| entry.tp.clone()).unwrap_or_else(|| {
            D::type_of_dom_val(&self.val_of_prim(p).unwrap().dom().unwrap())
        })
    }

}


/// The key trait that defines a domain
pub trait Domain: Clone + Debug + PartialEq + Eq + Hash {
    type Data: Debug + Default;

    fn val_of_prim_fallback(p: &Symbol) -> Option<Val<Self>>;

    fn type_of_dom_val(&self) -> Type;

    fn new_dsl() -> DSL<Self>;
}

