#![cfg(feature = "python")]

use pyo3::prelude::*;
use pyo3::types::PyList;
use pyo3::conversion::IntoPyObjectExt;

use crate::domains::py::PyVal;
use crate::eval;
type Val = eval::Val<PyVal>;
use PyVal::*;


pub fn val_to_py(py: Python<'_>, v: Val) -> PyResult<Py<PyAny>> {
    match v.dom().expect("Val should be Dom") {
        Int(i) => {
            Ok(i.into_py_any(py)?)
        }
        List(xs) => {
            let mut elems: Vec<Py<PyAny>> = Vec::with_capacity(xs.len());
            for x in xs {
                elems.push(val_to_py(py, x.clone())?);
            }

            let list_bound: Bound<'_, PyList> = PyList::new(py, &elems)?;

            Ok(list_bound.into_any().unbind())
        }
    }
}

pub fn py_to_val(obj: &Bound<'_, PyAny>) -> Result<Val, String> {
    if let Ok(i) = obj.extract::<i32>() {
        return Ok(Val::from(Int(i)));
    }
    if let Ok(list) = obj.downcast::<PyList>() {
        let mut out: Vec<Val> = Vec::with_capacity(list.len());
        for item in list.iter() {
            out.push(py_to_val(&item)?);
        }
        return Ok(Val::from(List(out)));
    }
    Err("unsupported Python type for PyVal".into())
}
