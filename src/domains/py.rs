use crate::*;

#[cfg(feature = "python")]
use pyo3::prelude::*;
#[cfg(feature = "python")]
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

// aliases of various typed specialized to our PyVal
type Val = crate::eval::Val<PyVal>;
type Evaluator<'a> = crate::eval::Evaluator<'a,PyVal>;
type VResult = crate::eval::VResult<PyVal>;
type Env = crate::eval::Env<PyVal>;


use PyVal::*;
impl FromVal<PyVal> for i32 {
    fn from_val(v: Val) -> Result<Self, VError> {
        match v {
            Dom(Int(i)) => Ok(i),
            _ => Err("from_val_to_i32: not an int".into())
        }
    }
}
impl<T: FromVal<PyVal>> FromVal<PyVal> for Vec<T> {
    fn from_val(v: Val) -> Result<Self, VError> {
        match v {
            Dom(List(v)) => v.into_iter().map(|v| T::from_val(v)).collect(),
            _ => Err("from_val_to_vec: not a list".into())
        }
    }
}

impl From<i32> for Val {
    fn from(i: i32) -> Val {
        Dom(Int(i))
    }
}
impl<T: Into<Val>> From<Vec<T>> for Val {
    fn from(vec: Vec<T>) -> Val {
        Dom(List(vec.into_iter().map(|v| v.into()).collect()))
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
        let mut prods = vec![];
        let dsl = DSL::new(prods);
        dsl
    }

    fn val_of_prim_fallback(p: &Symbol) -> Option<Val> {
        // if p.chars().next().unwrap().is_ascii_digit() {
        //     let i: i32 = p.parse().ok()?;
        //     Some(Int(i).into())
        // }
        // // starts with `[` -> List (must be all ints)
        // else if p.starts_with('[') {
        //     let intvec: Vec<i32> = serde_json::from_str(p).ok()?;
        //     let valvec: Vec<Val> = intvec.into_iter().map(|v|Dom(Int(v))).collect();
        //     Some(List(valvec).into())
        // } else {
        //     None
        // }
        None    // We will add this later if we want to
    }

    fn type_of_dom_val(&self) -> SlowType {
        match self {
            Int(_) => SlowType::base(Symbol::from("int")),
            List(xs) => {
                let elem_tp = if xs.is_empty() {
                    SlowType::Var(0) // (list t0)
                } else {
                    // todo here we just use the type of the first entry as the type
                    Self::type_of_dom_val(&xs.first().unwrap().clone().dom().unwrap())
                    // assert!(xs.iter().all(|v| Self::type_of_dom_val(v.clone().dom().unwrap())))
                };
                SlowType::Term("list".into(),vec![elem_tp])
            },
        }
    }

}