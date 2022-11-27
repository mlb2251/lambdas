

/// this macro is used at the start of a DSL function to load arguments out of their args vec
#[macro_export]
macro_rules! load_args {
    (   $args:expr,
        $($name:ident : $type:ty ),*
    ) => { 
        $(
            let $name:$type = <$type>::from_val($args.pop_front())?;
        )*
    }
}

/// this macro is used at the start of a DSL function to load arguments out of their args vec
#[macro_export]
macro_rules! load_arg {
    (   
        $handle: expr,
        $args:expr,
        $i:expr
    ) => {{ 
        let val = match load_arg_lazy!($args, $i) {
            Val::Thunk(idx,env) => &$handle.eval_child(*idx, &env)?,
            val => val
        };        
        FromVal::from_val(val)?
    }}
}

#[macro_export]
macro_rules! load_arg_lazy {
    (   
        $args:expr,
        $i:expr
    ) => {{ 
        $args.get($i)
    }}
}


