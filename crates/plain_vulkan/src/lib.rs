#[macro_use]
extern crate rustler;
//#[macro_use]
//extern crate rustler_codegen;

#[allow(unused_imports)]
use rustler::{Env, Term, Error, Encoder};

rustler_export_nifs!(
    "plain_vulkan",
    [("hello_world", 0, hello_world)],
    Some(on_load)
);

fn on_load(_env: Env, _info: Term) -> bool {
    true
}

fn hello_world<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok("hello world!".encode(env))
}
