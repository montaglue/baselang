use crate::{
    ast::Ast,
    utils::error::{CompilerResult, Errors},
};

pub mod typecheck;

pub fn resolve(ast: &mut Ast, errors: &mut Errors) -> CompilerResult<()> {
    typecheck::type_check(ast, errors)
}
