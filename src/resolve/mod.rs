use crate::{
    ast::Ast,
    utils::error::{CompilerResult, Errors},
};

use self::constructor_generation::constructor_generation;

pub mod constructor_generation;
pub mod typecheck;

pub fn resolve(ast: &mut Ast, errors: &mut Errors) -> CompilerResult<()> {
    constructor_generation(ast, errors)?;
    typecheck::type_check(ast, errors)
}
