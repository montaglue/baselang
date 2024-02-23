use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Ast, ExprId, ExprKind, TypeDef},
    utils::error::{error, CompilerResult, ErrorKind, Errors},
};

pub struct Functions {
    funs: HashSet<String>,
    struct_constructors: HashSet<String>,
    enum_constructors: HashMap<String, String>,
}

fn walk_ast(
    expr_id: ExprId,
    ast: &mut Ast,
    funs: &Functions,
    errors: &mut Errors,
) -> CompilerResult<()> {
    match ast[expr_id].kind {
        ExprKind::Add(exprs) => {
            for expr in exprs {
                walk_ast(expr, ast, funs, errors)?;
            }
        }
        ExprKind::Eq(lhs, rhs) => {
            walk_ast(lhs, ast, funs, errors)?;
            walk_ast(rhs, ast, funs, errors)?;
        }
        ExprKind::Not(expr) => {
            walk_ast(expr, ast, funs, errors)?;
        }
        ExprKind::Set(lhs, rhs) => {
            walk_ast(lhs, ast, funs, errors)?;
            walk_ast(rhs, ast, funs, errors)?;
        }
        ExprKind::Let(_, _, expr) => {
            walk_ast(expr, ast, funs, errors)?;
        }
        ExprKind::Print(expr) => {
            walk_ast(expr, ast, funs, errors)?;
        }
        ExprKind::Return(expr) => {
            walk_ast(expr, ast, funs, errors)?;
        }
        ExprKind::If(cond, then, els) => {
            walk_ast(cond, ast, funs, errors)?;
            for expr in then {
                walk_ast(expr, ast, funs, errors)?;
            }
            if let Some(els) = els {
                for expr in els {
                    walk_ast(expr, ast, funs, errors)?;
                }
            }
        }
        ExprKind::While(cond, body) => {
            walk_ast(cond, ast, funs, errors)?;
            for expr in body {
                walk_ast(expr, ast, funs, errors)?;
            }
        }
        ExprKind::New(expr) => {
            walk_ast(expr, ast, funs, errors)?;
        }
        ExprKind::Ref(rf) => {
            walk_ast(rf, ast, funs, errors)?;
        }
        ExprKind::Deref(dref) => {
            walk_ast(dref, ast, funs, errors)?;
        }
        ExprKind::Call(ref fun, exprs) => {
            if !funs.funs.contains(fun) {
                let fun_name = fun.clone();

                ast[expr_id].kind = if !funs.struct_constructors.contains(fun) {
                    let Some(enum_name) = funs.enum_constructors.get(fun) else {
                        return error(ErrorKind::NoSuchFunctionOrStruct(fun.to_owned()));
                    };

                    // assert that exprs.len == 1
                    ExprKind::EnumConstructor(fun_name, enum_name.clone(), exprs.0)
                } else {
                    ExprKind::StructConstructor(fun_name, exprs)
                };
            }

            for expr in exprs {
                walk_ast(expr, ast, funs, errors)?;
            }
        }

        _ => (),
    }

    Ok(())
}

pub fn constructor_generation(ast: &mut Ast, errors: &mut Errors) -> CompilerResult<()> {
    let mut funs = Functions {
        funs: HashSet::new(),
        struct_constructors: HashSet::new(),
        enum_constructors: HashMap::new(),
    };

    for fun in &ast.functions {
        funs.funs.insert(fun.name.clone());
    }

    for (name, def) in &ast.type_defs {
        if let TypeDef::Struct(_) = def {
            funs.struct_constructors.insert(name.clone());
        }
    }

    for (name, def) in &ast.type_defs {
        if let TypeDef::Enum(enm) = def {
            for (variant, _) in &enm.variants {
                funs.enum_constructors.insert(variant.clone(), name.clone());
            }
        }
    }

    let bodys: Vec<_> = ast.functions.iter().map(|f| f.body).collect();

    for body in bodys {
        for expr in body {
            walk_ast(expr, ast, &funs, errors)?;
        }
    }

    Ok(())
}
