use crate::*;

use std::collections::{HashMap, HashSet};
use std::fmt::{Debug};
use std::hash::Hash;
use std::sync::Arc;

#[cfg(feature = "python")]
use pyo3::prelude::*;
#[cfg(feature = "python")]
use pyo3::types::{PyList, PyTuple};

pub type DSLFn<D> = fn(Env<D>, &Evaluator<D>) -> VResult<D>;

#[derive(Clone)]
pub enum FnPtr<D: Domain> {
    Native(DSLFn<D>),
    #[cfg(feature = "python")]
    Python(Arc<Py<PyAny>>),
}

#[derive(Clone)]
pub struct Production<D: Domain> {
    pub name: Symbol, // eg "map" or "0" or "[1,2,3]"
    pub val: Val<D>,
    pub tp: SlowType,
    pub arity: usize,
    pub lazy_args: HashSet<usize>,
    pub fn_ptr: Option<FnPtr<D>>,
}

impl<D:Domain> Debug for Production<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Production").field("name", &self.name).field("val", &self.val).field("tp", &self.tp).field("arity", &self.arity).finish()
    }
}

impl<D: Domain> Production<D> {
    #[inline]
    pub fn call(&self, args: Env<D>, handle: &Evaluator<D>) -> VResult<D> {
        match &self.fn_ptr{
            Some(FnPtr::Native(f)) => f(args, handle),

            #[cfg(feature = "python")]
            Some(FnPtr::Python(pyf)) => {
                Python::with_gil(|py| -> Result<Val<D>, String> {
                    // Env<D> -> Python list
                    let mut elems: Vec<Py<PyAny>> = Vec::with_capacity(args.len());
                    for v in &args.env {
                        let obj = D::py_val_to_py(py, v.clone())
                            .map_err(|e| e.to_string())?;
                        elems.push(obj);
                    }

                    let tuple_bound = PyTuple::new(py, &elems)  // Bound<PyTuple>
                            .map_err(|e| e.to_string())?;

                    let ret = (**pyf)
                        .bind(py)
                        .call1(tuple_bound)
                        .map_err(|e| e.to_string())?;

                    // Python -> Val<D>
                    D::py_py_to_val(&ret)
                })
            }
            None => Err("primitive has no function pointer".into()),
        }
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
        Production::func_custom(name, tp, Default::default(), fn_ptr)
    }

    pub fn func_custom(name: &str, tp: &str, lazy_args: Option<&[usize]>, fn_ptr: DSLFn<D>) -> Self {
        let lazy_args = lazy_args.map(|args|args.iter().copied().collect()).unwrap_or_default();
        Production::func_raw(name.into(), tp.parse().unwrap(), lazy_args, fn_ptr)
    }

    pub fn val_raw(name: Symbol, tp: SlowType, val: Val<D>) -> Self {
        assert_eq!(tp.arity(),0);
        Production {
            name,
            val,
            tp,
            arity: 0,
            lazy_args: Default::default(),
            fn_ptr: None,
        }
    }

    pub fn func_raw(name: Symbol, tp: SlowType, lazy_args: HashSet<usize>, fn_ptr: DSLFn<D>) -> Self {
        let arity = tp.arity();
        Production {
            name: name.clone(),
            val: PrimFun(CurriedFn::<D>::new(name, arity)),
            tp,
            arity,
            lazy_args,
            fn_ptr: Some(FnPtr::Native(fn_ptr)),
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

    /// NEW: add a constant (arity 0) to the DSL
    pub fn add_constant(&mut self, name: Symbol, tp: SlowType, val: Val<D>) {
        assert_eq!(tp.arity(), 0);
        let prod = Production::val_raw(name, tp, val);
        self.add_entry(prod);
    }

    /// given a primitive's symbol return a runtime Val object. For function primitives
    /// this should return a PrimFun(CurriedFn) object.
    pub fn val_of_prim(&self, p: &Symbol) -> Option<Val<D>> {
        self.productions.get(p).map(|entry| entry.val.clone()).or_else(||
            D::val_of_prim_fallback(p))
    }

    pub fn type_of_prim(&self, p: &Symbol) -> SlowType {
        self.productions.get(p).map(|entry| entry.tp.clone()).unwrap_or_else(|| {
            D::type_of_dom_val(&self.val_of_prim(p).unwrap().dom().unwrap())
        })
    }
}


/// The key trait that defines a domain
pub trait Domain: Clone + Debug + PartialEq + Eq + Hash + Send + Sync {
    type Data: Debug + Default;

    fn val_of_prim_fallback(p: &Symbol) -> Option<Val<Self>>;

    fn type_of_dom_val(&self) -> SlowType;

    fn new_dsl() -> DSL<Self>;

    #[cfg(feature = "python")]
    fn py_val_to_py(py: Python<'_>, v: Val<Self>) -> PyResult<Py<PyAny>> {
        // default: not supported for this domain
        Err(pyo3::exceptions::PyTypeError::new_err(
            "Python bridge not implemented for this domain",
        ))
    }

    #[cfg(feature = "python")]
    fn py_py_to_val(_obj: &Bound<'_, PyAny>) -> Result<Val<Self>, String> {
        Err("Python bridge not implemented for this domain".into())
    }

}

