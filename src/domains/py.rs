use crate::*;
use std::collections::HashSet;
use std::sync::Arc;


use pyo3::prelude::*;
use pyo3::types::PyAny;

#[derive(Clone,Debug, PartialEq, Eq, Hash)]
pub enum PyVal {
    Int(i32),
    List(Vec<Val>),
}

#[derive(Clone,Debug, PartialEq, Eq, Hash)]
pub enum PyType {
    TInt,
    TList
}


type Val = crate::eval::Val<PyVal>;


#[cfg(feature = "python")]
pub fn create_python_production<D: Domain>(
    name: Symbol,
    tp: SlowType,
    lazy_args: Option<&[usize]>,
    pyfunc: Py<PyAny>,
) -> Production<D> {
    let arity = tp.arity();
    let lazy: HashSet<usize> = lazy_args
        .map(|xs| xs.iter().copied().collect())
        .unwrap_or_default();

    use crate::eval::{CurriedFn, Val};

    Production {
        name: name.clone(),
        val: Val::PrimFun(CurriedFn::<D>::new(name.clone(), arity)),
        tp,
        arity,
        lazy_args: lazy,
        fn_ptr: Some(FnPtr::Python(Arc::new(pyfunc)))
    }
}

// From<Val> impls are needed for unwrapping values. We can assume the program
// has been type checked so it's okay to panic if the type is wrong. Each val variant
// must map to exactly one unwrapped type (though it doesnt need to be one to one in the
// other direction)
impl FromVal<PyVal> for i32 {
    fn from_val(v: Val) -> Result<Self, VError> {
        match v {
            Dom(PyVal::Int(i)) => Ok(i),
            _ => Err("from_val_to_i32: not an int".into())
        }
    }
}
impl<T: FromVal<PyVal>> FromVal<PyVal> for Vec<T> {
    fn from_val(v: Val) -> Result<Self, VError> {
        match v {
            Dom(PyVal::List(v)) => v.into_iter().map(|v| T::from_val(v)).collect(),
            _ => Err("from_val_to_vec: not a list".into())
        }
    }
}

impl From<i32> for Val {
    fn from(i: i32) -> Val {
        Dom(PyVal::Int(i))
    }
}
impl<T: Into<Val>> From<Vec<T>> for Val {
    fn from(vec: Vec<T>) -> Val {
        Dom(PyVal::List(vec.into_iter().map(|v| v.into()).collect()))
    }
}

impl Domain for PyVal {
    type Data = ();

    #[cfg(feature = "python")]
    fn py_val_to_py(py: Python<'_>, v: crate::eval::Val<Self>) -> PyResult<Py<PyAny>> {
        crate::domains::simple_python::val_to_py(py, v)
    }

    #[cfg(feature = "python")]
    fn py_py_to_val(obj: &Bound<'_, PyAny>) -> Result<crate::eval::Val<Self>, String> {
        crate::domains::simple_python::py_to_val(obj)
    }

    fn new_dsl() -> DSL<Self> {
        let prods = vec![];
        let dsl = DSL::new(prods);
        dsl
    }

    fn val_of_prim_fallback(p: &Symbol) -> Option<Val> {
        None
    }

    fn type_of_dom_val(&self) -> SlowType {
        match self {
            PyVal::Int(_) => SlowType::base(Symbol::from("int")),
            PyVal::List(xs) => {
                let elem_tp = if xs.is_empty() {
                    SlowType::Var(0) // (list t0)
                } else {
                    let result = Self::type_of_dom_val(&xs.first().unwrap().clone().dom().unwrap());
                    assert!(xs.iter().all(|v| result == Self::type_of_dom_val(&v.clone().dom().unwrap())));
                    result
                };
                SlowType::Term("list".into(),vec![elem_tp])
            },
        }
    }

}