// The primitive list domain from Josh Rule's thesis, p.170.

use crate::*;

#[derive(Clone,Debug, PartialEq, Eq, Hash)]
pub enum ListVal {
    Int(i32),
    // Nan,  // TODO to model NAN or not...(josh rule dsl does it for things outside of 0-99)
    Bool(bool),
    List(Vec<Val>),
}

// In this domain, we limit how many times "fix" can be invoked.
// This is a crude way of finding infinitely looping programs.
const MAX_FIX_INVOCATIONS: u32 = 20;

type Val = crate::eval::Val<ListVal>;
type Evaluator<'a> = crate::eval::Evaluator<'a,ListVal>;
type VResult = crate::eval::VResult<ListVal>;
type Env = crate::eval::Env<ListVal>;
use ListVal::*;


// From<Val> impls are needed for unwrapping values. We can assume the program
// has been type checked so it's okay to panic if the type is wrong. Each val variant
// must map to exactly one unwrapped type (though it doesnt need to be one to one in the
// other direction)
impl FromVal<ListVal> for i32 {
    fn from_val(v: Val) -> Result<Self, VError> {
        match v {
            Dom(Int(i)) => Ok(i),
            _ => Err("from_val_to_list: not an int".into())
        }
    }
}
impl FromVal<ListVal> for bool {
    fn from_val(v: Val) -> Result<Self, VError> {
        match v {
            Dom(Bool(b)) => Ok(b),
            _ => Err("from_val_to_bool: not a bool".into())
        }
    }
}
impl<T: FromVal<ListVal>> FromVal<ListVal> for Vec<T>
{
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
impl From<bool> for Val {
    fn from(b: bool) -> Val {
        Dom(Bool(b))
    }
}
impl<T: Into<Val>> From<Vec<T>> for Val {
    fn from(vec: Vec<T>) -> Val {
        Dom(List(vec.into_iter().map(|v| v.into()).collect()))
    }
}

fn parse_vec(vec: &[serde_json::value::Value]) -> Vec<Val> {
    let valvec: Vec<Val> = vec.iter().map(|v| {
        if let Some(i) = v.as_i64() {
            Dom(Int(i as i32))
        } else if let Some(b) = v.as_bool() {
            Dom(Bool(b))
        } else {
            // not int, not bool -> must be array. If not, we error.
            // TODO make this spit out a more useful error than panic
            let arr = v.as_array().unwrap();
            Dom(List(parse_vec(arr)))
        }
    }).collect();
    valvec
}

#[derive(Default,Debug)]
pub struct ListData {
    fix_counter: u32,
}

impl Domain for ListVal {

    type Data = ListData;  // Use Data as fix-point invocation counter

    fn new_dsl() -> DSL<Self> {
        DSL::new(vec![
            Production::func("cons", "t0 -> list t0 -> list t0", cons),
            Production::func("+", "int -> int -> int", add),
            Production::func("-", "int -> int -> int", sub),
            Production::func(">", "int -> int -> bool", gt),
            Production::func_lazy("if", "bool -> t0 -> t0 -> t0", &[1,2], branch),
            Production::func("==", "t0 -> t0 -> bool", eq),
            Production::func("is_empty", "list t0 -> bool", is_empty),
            Production::func("head", "list t0 -> t0", head),
            Production::func("tail", "list t0 -> list t0", tail),
            // note in historical origami logs dreamcoder actually uses the signature: t0 -> ((t0 -> t1) -> t0 -> t1) -> t1    fix1
            // which is why we include fix_flip to use that order of arguments
            Production::func("fix_flip", "t0 -> ((t0 -> t1) -> t0 -> t1) -> t1", fix_flip),
            Production::func("fix", "((t0 -> t1) -> t0 -> t1) -> t0 -> t1", fix),
            Production::val("0", "int", Dom(Int(0))),
            Production::val("1", "int", Dom(Int(1))),
            Production::val("[]", "list t0", Dom(List(vec![]))),
        ])
    }

    // This is a fallback function used for supporting infinite DSLs, for example here we support all integers
    // and all integer lists. This function is called when a symbol isn't found in the DSL.
    // A simple default implementation is just to return `None`.
    fn val_of_prim_fallback(p: &Symbol) -> Option<Val> {
        // starts with digit or negative sign -> Int
        if p.chars().next().unwrap().is_ascii_digit() || p.starts_with('-') {
            let i: i32 = p.parse().ok()?;
            Some(Int(i).into())
        }
        // starts with "f" or "t" -> must be a bool (if not found in PRIMS)
        else if p.starts_with('f') || p.starts_with('t') {
            let s: String = p.parse().ok()?;
            if s == "false" {
                Some(Dom(Bool(false)))
            } else if s == "true" {
                Some(Dom(Bool(true)))
            } else {
                None
            }
        }
        // starts with `[` -> List
        // Note lists may contain ints, bools, or other lists in this domain
        else if p.starts_with('[') {
            let elems: Vec<serde_json::value::Value> = serde_json::from_str(p).ok()?;
            let valvec: Vec<Val> = parse_vec(&elems);
            Some(List(valvec).into())
        } else {
            None
        }
    }

    // gets the type of a value
    fn type_of_dom_val(&self) -> SlowType {
        match self {
            Int(_) => SlowType::base(Symbol::from("int")),
            Bool(_) =>  SlowType::base("bool".into()),
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

// *********************
// *** DSL FUNCTIONS ***
// *********************

fn cons(mut args: Env, _handle: &Evaluator) -> VResult {
    load_args!(args, x:Val, xs:Vec<Val>); 
    let mut rxs = xs;
    rxs.insert(0, x);
    // println!("{:?}", rxs);
    ok(rxs)
}

fn add(mut args: Env, _handle: &Evaluator) -> VResult {
    load_args!(args, x:i32, y:i32); 
    ok(x+y)
}

fn sub(mut args: Env, _handle: &Evaluator) -> VResult {
    load_args!(args, x:i32, y:i32); 
    ok(x-y)
}

fn gt(mut args: Env, _handle: &Evaluator) -> VResult {
    load_args!(args, x:i32, y:i32); 
    ok(x>y)
}

fn branch(mut args: Env, handle: &Evaluator) -> VResult {
    load_args!(args, b: bool, tbranch: Val, fbranch: Val); 
    if b { 
        tbranch.unthunk(handle)
    } else { 
        fbranch.unthunk(handle)
    }
}

fn eq(mut args: Env, _handle: &Evaluator) -> VResult {
    load_args!(args, x:Val, y:Val); 
    ok(x == y) // since Vals have Eq implemented already in the way that we want
}

fn is_empty(mut args: Env, _handle: &Evaluator) -> VResult {
    load_args!(args, xs: Vec<Val>);
    ok(xs.is_empty())
}

fn head(mut args: Env, _handle: &Evaluator) -> VResult {
    load_args!(args, xs: Vec<Val>);
    if xs.is_empty() {
        Err(String::from("head called on empty list"))
    } else {
        ok(xs[0].clone())
    }
}

fn tail(mut args: Env, _handle: &Evaluator) -> VResult {
    load_args!(args, xs: Vec<Val>);
    if xs.is_empty() {
        Err(String::from("tail called on empty list"))
    } else {
        ok(xs[1..].to_vec())
    }
}

use once_cell::sync::Lazy;
pub static FIX: Lazy<Val> = Lazy::new(|| PrimFun(CurriedFn::new(Symbol::from("fix"), 2)));


/// fix f x = f(fix f)(x)
/// type i think: ((t0 -> t1) -> t0 -> t1) -> t0 -> t1 
fn fix(mut args: Env, handle: &Evaluator) -> VResult {
    handle.data.borrow_mut().fix_counter += 1;
    if handle.data.borrow().fix_counter > MAX_FIX_INVOCATIONS {
        return Err(format!("Exceeded max number of fix invocations. Max was {}", MAX_FIX_INVOCATIONS));
    }
    load_args!(args, fn_val: Val, x: Val);

    // fix f x = f(fix f)(x)
    // let fixf = PrimFun(CurriedFn::new_with_args(Symbol::from("fix"), 2, vec![Val::new_strict(fn_val.clone())]));
    let fixf = handle.apply(&FIX, fn_val.clone()).unwrap();
    let res = match handle.apply(&fn_val, fixf) {
        Ok(ffixf) => handle.apply(&ffixf, x),
        Err(err) => Err(format!("Could not apply fixf to f: {}",err))
    };
    handle.data.borrow_mut().fix_counter -= 1;
    res
}


/// fix x f = f(fix f)(x)
/// type i think: t0 -> ((t0 -> t1) -> t0 -> t1) -> t1 
/// This is to match dreamcoder.
fn fix_flip(mut args: Env, handle: &Evaluator) -> VResult {
    // load_args!(args, x: Val, fn_val: Val);

    // // fn_val = \f \xs ... so  we can look one layer in to
    // // get the \xs function
    // let lam_xs = match &fn_val {
    //     LamClosure(body, _) => *body,
    //     _ => return Err(format!("fix_flip called on non-lambda"))
    // };


    

    args.reverse();
    fix(args, handle)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_prim_lists() {

        let dsl = ListVal::new_dsl();

        let arg = dsl.val_of_prim(&"[]".into()).unwrap();
        assert_execution::<ListVal, Vec<Val>>("(if (is_empty $0) $0 (tail $0))", &[arg], vec![]);

        // test cons
        let arg = dsl.val_of_prim(&"[1,2,3]".into()).unwrap();
        assert_execution("(cons 0 $0)", &[arg], vec![0,1,2,3]);

        // test +
        assert_execution::<ListVal, i32>("(+ 1 2)", &[], 3);

        // test -
        assert_execution::<ListVal, i32>("(- 22 1)", &[], 21);

        // test >
        assert_execution::<ListVal, bool>("(> 22 1)", &[], true);
        assert_execution::<ListVal, bool>("(> 2 11)", &[], false);

        // test if
        assert_execution::<ListVal, i32>("(if true 5 50)", &[], 5);
        assert_execution::<ListVal, i32>("(if false 5 50)", &[], 50);

        // test ==
        assert_execution::<ListVal, bool>("(== 5 5)", &[], true);
        assert_execution::<ListVal, bool>("(== 5 50)", &[], false);
        let arg1 = dsl.val_of_prim(&"[[],[3],[4,5]]".into()).unwrap();
        let arg2 = dsl.val_of_prim(&"[[],[3],[4,5]]".into()).unwrap();
        assert_execution::<ListVal, bool>("(== $0 $1)", &[arg1, arg2], true);
        let arg1 = dsl.val_of_prim(&"[[],[3],[4,5]]".into()).unwrap();
        let arg2 = dsl.val_of_prim(&"[[3],[4,5]]".into()).unwrap();
        assert_execution::<ListVal, bool>("(== $0 $1)", &[arg1, arg2], false);
        let arg1 = dsl.val_of_prim(&"[[]]".into()).unwrap();
        let arg2 = dsl.val_of_prim(&"[]".into()).unwrap();
        assert_execution::<ListVal, bool>("(== $0 $1)", &[arg1, arg2], false);
        let arg1 = dsl.val_of_prim(&"[]".into()).unwrap();
        let arg2 = dsl.val_of_prim(&"[]".into()).unwrap();
        assert_execution::<ListVal, bool>("(== $0 $1)", &[arg1, arg2], true);

        // test is_empty
        let arg = dsl.val_of_prim(&"[[],[3],[4,5]]".into()).unwrap();
        assert_execution("(is_empty $0)", &[arg], false);
        let arg = dsl.val_of_prim(&"[]".into()).unwrap();
        assert_execution("(is_empty $0)", &[arg], true);

        // test head
        let arg = dsl.val_of_prim(&"[[1,2],[3],[4,5]]".into()).unwrap();
        assert_execution("(head $0)", &[arg], vec![1,2]);

        // test tail
        let arg = dsl.val_of_prim(&"[[1,2],[3],[4,5]]".into()).unwrap();
        assert_execution("(tail $0)", &[arg], vec![vec![3], vec![4, 5]]);
        let arg = dsl.val_of_prim(&"[[1,2]]".into()).unwrap();
        assert_execution::<ListVal, Vec<Val>>("(tail $0)", &[arg], vec![]);

        // test fix
        let arg = dsl.val_of_prim(&"[]".into()).unwrap();
        assert_execution("(fix_flip $0 (lam (lam (if (is_empty $0) 0 (+ 1 ($1 (tail $0)))))))", &[arg], 0);
        let arg = dsl.val_of_prim(&"[1,2,3,2,1]".into()).unwrap();
        assert_execution("(fix_flip $0 (lam (lam (if (is_empty $0) 0 (+ 1 ($1 (tail $0)))))))", &[arg], 5);
        let arg = dsl.val_of_prim(&"[1,2,3,4,5]".into()).unwrap();
        assert_execution("(fix_flip $0 (lam (lam (if (is_empty $0) $0 (cons (+ 1 (head $0)) ($1 (tail $0)))))))", &[arg], vec![2, 3, 4, 5, 6]);
        let arg = dsl.val_of_prim(&"[1,2,3,4,5]".into()).unwrap();
        assert_error::<ListVal, Val>(
            "(fix_flip $0 (lam (lam (if (is_empty $0) $0 (cons (+ 1 (head $0)) ($1 $0))))))",
            &[arg],
            format!("Exceeded max number of fix invocations. Max was {}", MAX_FIX_INVOCATIONS));
    }
}