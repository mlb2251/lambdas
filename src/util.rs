use crate::*;
use std::fmt::Debug;

/// convenience function for returning arguments from a DSL function
pub fn ok<T: Into<Val<D>> , D:Domain>(v: T) -> VResult<D> {
    Ok(v.into())
}

/// convenience function for equality assertions
pub fn assert_eq_val<D:Domain, T>(v: &Val<D>, o: T)
where T: FromVal<D> + Debug + PartialEq,
{
    assert_eq!(T::from_val(v.clone()).unwrap(), o);
}

/// convenience function for asserting that something executes to what you'd expect
pub fn assert_execution<D: Domain, T>(expr: &str, args: &[Val<D>], expected: T)
where T: FromVal<D> + Debug + PartialEq,
{
    let mut set = ExprSet::empty(Order::ChildFirst, false, false);
    let e = set.parse_extend(expr).unwrap();
    let mut args: Vec<Thunk<D>> = args.iter().map(|arg|Thunk::from_val(arg.clone())).collect();
    let res = set.get(e).eval(&mut args, &D::new_dsl(), None).unwrap();
    assert_eq_val(&res,expected);
}

pub fn assert_error<D: Domain, T>(expr: &str, args: &[Val<D>], expected_error_msg: String)
where T: FromVal<D> + Debug + PartialEq
{
    let mut set = ExprSet::empty(Order::ChildFirst, false, false);
    let e = set.parse_extend(expr).unwrap();
    let mut args: Vec<Thunk<D>> = args.iter().map(|arg|Thunk::from_val(arg.clone())).collect();
    let res = set.get(e).eval(&mut args, &D::new_dsl(), None);
    assert!(res.is_err());
    assert_eq!(expected_error_msg, res.err().unwrap());
}