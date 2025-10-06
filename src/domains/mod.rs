
// just register each domain here by including it with `pub mod domain_name;`
pub mod simple;
pub mod prim_lists;

#[cfg(feature = "python")]
pub mod simple_python; // <-- gate the module declaration
