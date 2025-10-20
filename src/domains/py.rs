use crate::*;

#[cfg(feature = "python")]
use pyo3::prelude::*;
// #[cfg(feature = "python")]
// use pyo3::types::PyTuple;
#[cfg(feature = "python")]
use pyo3::types::PyAny;

/// by dreamegg::domain::Val so they don't appear here.
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

// aliases of various typed specialized to our SimpleVal
type Val = crate::eval::Val<PyVal>;
type Evaluator<'a> = crate::eval::Evaluator<'a,PyVal>;
type VResult = crate::eval::VResult<PyVal>;
type Env = crate::eval::Env<PyVal>;

// to more concisely refer to the variants
use PyVal::*;

// From<Val> impls are needed for unwrapping values. We can assume the program
// has been type checked so it's okay to panic if the type is wrong. Each val variant
// must map to exactly one unwrapped type (though it doesnt need to be one to one in the
// other direction)
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

// These Into<Val>s are convenience functions. It's okay if theres not a one to one mapping
// like this in all domains - it just makes .into() save us a lot of work if there is.
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

// here we actually implement Domain for our domain. 
impl Domain for PyVal {
    // we dont use Data here
    type Data = ();

    #[cfg(feature = "python")]
    fn py_val_to_py(py: Python<'_>, v: crate::eval::Val<Self>) -> PyResult<Py<PyAny>> {
        // delegate to your existing converter
        crate::domains::simple_python::val_to_py(py, v)
    }

    #[cfg(feature = "python")]
    fn py_py_to_val(obj: &Bound<'_, PyAny>) -> Result<crate::eval::Val<Self>, String> {
        // delegate to your existing converter
        crate::domains::simple_python::py_to_val(obj)
    }

    fn new_dsl() -> DSL<Self> {
        let mut prods = vec![
            // Production::func("+", "int -> int -> int", add),
            // Production::func("*", "int -> int -> int", mul),
            // Production::func("map", "(t0 -> t1) -> (list t0) -> (list t1)", map),
            // Production::func("sum", "list int -> int", sum),
            // Production::val("0", "int", Dom(Int(0))),
            // Production::val("1", "int", Dom(Int(1))),
            // Production::val("2", "int", Dom(Int(2))),
            // Production::val("[]", "(list t0)", Dom(List(vec![]))),
        ];

        // // Log the built-ins you just added
        // eprintln!("[DSL] built-ins:");
        // // eprintln!("  func * : int -> int -> int");
        // eprintln!("  func map : (t0 -> t1) -> (list t0) -> (list t1)");
        // eprintln!("  val 0 : int");
        // eprintln!("  val 1 : int");
        // eprintln!("  val 2 : int");
        // eprintln!("  val [] : (list t0)");

        let dsl = DSL::new(prods);
        // eprintln!("[DSL] build complete.");
        dsl
    }

    // val_of_prim takes a symbol like "+" or "0" and returns the corresponding Val.
    // Note that it can largely just be a call to the global hashmap PRIMS that define_semantics generated
    // however you're also free to do any sort of generic parsing you want, allowing for domains with
    // infinite sets of values or dynamically generated values. For example here we support all integers
    // and all integer lists.
    fn val_of_prim_fallback(p: &Symbol) -> Option<Val> {
        // starts with digit -> Int
        if p.chars().next().unwrap().is_ascii_digit() {
            let i: i32 = p.parse().ok()?;
            Some(Int(i).into())
        }
        // starts with `[` -> List (must be all ints)
        else if p.starts_with('[') {
            let intvec: Vec<i32> = serde_json::from_str(p).ok()?;
            let valvec: Vec<Val> = intvec.into_iter().map(|v|Dom(Int(v))).collect();
            Some(List(valvec).into())
        } else {
            None
        }
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