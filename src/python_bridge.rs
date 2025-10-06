// // lambdas/src/python_bridge.rs

// #![cfg(feature = "python")]

// use once_cell::sync::OnceCell;
// use std::cell::RefCell;
// use std::collections::HashMap;
// use std::sync::Mutex;
// use pyo3::prelude::*;
// use pyo3::Py;
// use pyo3::prelude::*;
// use pyo3::types::{PyDict, PyTuple};

// /// symbol -> Python callable
// static CALLABLES: OnceCell<Mutex<HashMap<String, Py<PyAny>>>> = OnceCell::new();

// /// (symbol, type_str, _reg_name) list; we’ll dispatch by *symbol*
// static PRIMS: OnceCell<Mutex<Vec<(String, String, String)>>> = OnceCell::new();

// fn callables() -> &'static Mutex<HashMap<String, Py<PyAny>>> {
//     CALLABLES.get_or_init(|| Mutex::new(HashMap::new()))
// }
// fn prims() -> &'static Mutex<Vec<(String, String, String)>> {
//     PRIMS.get_or_init(|| Mutex::new(Vec::new()))
// }

// /// Thread-local “which symbol is currently executing”
// thread_local! {
//     static CURRENT_SYMBOL: RefCell<Option<String>> = RefCell::new(None);
// }

// pub fn set_current_symbol(sym: &str) {
//     CURRENT_SYMBOL.with(|s| *s.borrow_mut() = Some(sym.to_string()));
// }
// pub fn clear_current_symbol() {
//     CURRENT_SYMBOL.with(|s| *s.borrow_mut() = None);
// }
// pub fn current_symbol() -> Option<String> {
//     CURRENT_SYMBOL.with(|s| s.borrow().clone())
// }

// pub fn clone_callable(symbol: &str, py: Python<'_>) -> PyResult<Py<PyAny>> {
//     let m = callables().lock().unwrap();
//     match m.get(symbol) {
//         Some(obj) => Ok(obj.clone_ref(py)), // <- correct way to duplicate Py<PyAny>
//         None => Err(pyo3::exceptions::PyKeyError::new_err(
//             format!("no python callable for symbol `{symbol}`"),
//         )),
//     }
// }

// /// Python calls this to register a callable for a *symbol*.
// /// Use the *symbol* as the key you’ll write in DSL programs, e.g. "add".
// pub fn register_callable(symbol: String, func: &Bound<PyAny>) -> PyResult<()> {
//     let owned: Py<PyAny> = func.clone().unbind();
//     let mut m = callables().lock().unwrap();
//     m.insert(symbol, owned);
//     Ok(())
// }

// pub fn register_primitive(symbol: String, ty: String, registry_name: String) -> PyResult<()> {
//     let mut v = prims().lock().unwrap();
//     v.push((symbol, ty, registry_name));
//     Ok(())
// }

// /// Take and clear the primitive list (so we won’t double-append)
// pub fn prims_take() -> Vec<(String, String, String)> {
//     std::mem::take(&mut *prims().lock().unwrap())
// }

// /// Low-level call by symbol: (*args, **kwargs) → PyObject
// pub fn call_by_symbol_raw(
//     symbol: &str,
//     py: Python<'_>,
//     args: &Bound<PyTuple>,
//     kwargs: Option<&Bound<PyDict>>,
// ) -> PyResult<PyObject> {
//     // Clone while not holding Python or calling back
//     let f: Py<PyAny> = {
//         let map = callables().lock().unwrap();
//         match map.get(symbol) {
//             Some(obj) => obj.clone_ref(py),
//             None => {
//                 return Err(pyo3::exceptions::PyKeyError::new_err(
//                     format!("no python callable for symbol `{symbol}`"),
//                 ))
//             }
//         }
//     };
//     let f = f.bind(py);
//     let out = f.call(args, kwargs)?;
//     Ok(out.unbind().into())
// }
