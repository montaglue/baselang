use crate::{
    ast::{Ast, AstFunctionId, AstType, ExprId, ExprKind},
    utils::error::{error, CompilerError, CompilerResult, ErrorKind, Errors},
};

use self::context::TypeContext;

pub mod context;
pub fn type_derive_expr(
    expr_id: ExprId,
    ast: &mut Ast,
    context: &mut TypeContext,
    errors: &mut Errors,
) -> CompilerResult<AstType> {
    let expr = ast[expr_id].clone();

    let res = match expr.kind {
        ExprKind::Add(exprs) => {
            let types: CompilerResult<Vec<AstType>> = exprs
                .map(|expr| type_derive_expr(expr, ast, context, errors))
                .collect();
            let types = types?;
            let last = types.last().unwrap();
            if !types.iter().all(|typ| ast.t_eq(*typ, *last)) {
                return error(ErrorKind::SumIncompatibleTypes);
            }
            *last
        }
        ExprKind::Eq(lhs, rhs) => {
            let lhs = type_derive_expr(lhs, ast, context, errors)?;
            type_check_expr(rhs, ast, lhs, context, errors)?;
            AstType::Bool
        }
        ExprKind::Not(expr) => {
            type_check_expr(expr, ast, AstType::Bool, context, errors)?;
            AstType::Bool
        }
        ExprKind::Set(ptr, value) => {
            let value_type = type_derive_expr(value, ast, context, errors)?;

            type_check_expr(ptr, ast, value_type, context, errors)?;

            AstType::Unit
        }
        ExprKind::Print(expr) => {
            let _typ = type_derive_expr(expr, ast, context, errors)?;
            AstType::Unit
        }
        ExprKind::Integer(_) => AstType::Int,
        ExprKind::Float(_) => AstType::Float,
        ExprKind::Bool(_) => AstType::Bool,
        ExprKind::String(_) => AstType::String,
        ExprKind::Ident(var) => context
            .variables
            .get(&var)
            .cloned()
            .ok_or(CompilerError::new(ErrorKind::NoSuchVariable(var)))?,

        ExprKind::Call(fun, args) => {
            let Some(AstType::Function(args_types, ret_type)) =
                context.functions.get(&fun).cloned()
            else {
                return error(ErrorKind::NoSuchVariable(fun));
            };
            for (arg, arg_type) in args.zip(args_types) {
                type_check_expr(arg, ast, ast[arg_type], context, errors)?;
            }
            ast[ret_type]
        }
        ExprKind::Return(expr) => {
            type_check_expr(
                expr,
                ast,
                context.return_type.clone().unwrap(),
                context,
                errors,
            )?;
            AstType::Never
        }
        ExprKind::If(cond, then, els) => {
            type_check_expr(cond, ast, AstType::Bool, context, errors)?;
            for expr in then {
                type_check_expr(expr, ast, AstType::Unit, context, errors)?;
            }
            if let Some(els) = els {
                for expr in els {
                    type_check_expr(expr, ast, AstType::Unit, context, errors)?;
                }
            }
            AstType::Unit
        }
        ExprKind::While(cond, body) => {
            type_check_expr(cond, ast, AstType::Bool, context, errors)?;
            for expr in body {
                type_check_expr(expr, ast, AstType::Unit, context, errors)?;
            }
            AstType::Unit
        }
        ExprKind::New(expr) => {
            let typ = type_derive_expr(expr, ast, context, errors)?;
            let typ = ast.add_type(typ);
            AstType::Pointer(typ)
        }
        ExprKind::Ref(id) => {
            let typ = type_derive_expr(id, ast, context, errors)?;
            let typ = ast.add_type(typ);
            AstType::Pointer(typ)
        }
        ExprKind::Deref(id) => {
            let typ = type_derive_expr(id, ast, context, errors)?;
            if let AstType::Pointer(typ) = typ {
                ast[typ]
            } else {
                return error(ErrorKind::UnexpectedType(format!("{:?}", typ)));
            }
        }
        ExprKind::Let(name, typ, expr) => {
            let typ = type_derive_expr(expr, ast, context, errors)?;
            context.add_variable(name, typ.clone());
            AstType::Unit
        }
    };

    ast[expr.typ] = res;

    Ok(res)
}

pub fn type_check_expr(
    expr_id: ExprId,
    ast: &mut Ast,
    suggestion: AstType,
    context: &mut TypeContext,
    errors: &mut Errors,
) -> CompilerResult<()> {
    let expr = ast[expr_id].clone();
    ast[expr.typ] = suggestion.clone();
    match &expr.kind {
        ExprKind::Add(exprs) => {
            for expr in *exprs {
                type_check_expr(expr, ast, suggestion.clone(), context, errors)?;
            }
            Ok(())
        }
        ExprKind::Eq(lhs, rhs) => {
            let lhs_type = type_derive_expr(*lhs, ast, context, errors)?;
            type_check_expr(*rhs, ast, lhs_type, context, errors)?;

            if ast.t_eq(suggestion, AstType::Bool) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Bool,
                    got: suggestion,
                })
            }
        }
        ExprKind::Not(expr) => {
            type_check_expr(*expr, ast, AstType::Bool, context, errors)?;
            if ast.t_eq(suggestion, AstType::Bool) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Bool,
                    got: suggestion,
                })
            }
        }
        ExprKind::Set(ptr, value) => {
            let typ = type_derive_expr(*value, ast, context, errors)?;
            if let ExprKind::Ident(name) = &ast[*ptr].kind {
                context.variables.insert(name.clone(), typ.clone());
            }
            Ok(())
        }
        ExprKind::Print(expr) => {
            let _typ = type_derive_expr(*expr, ast, context, errors)?;
            if ast.t_eq(suggestion, AstType::Unit) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Unit,
                    got: suggestion,
                })
            }
        }
        ExprKind::Integer(_) => {
            if ast.t_eq(suggestion, AstType::Int) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Int,
                    got: suggestion,
                })
                .map_err(|e| e.message(format!("expr: {:?}", expr)))
            }
        }
        ExprKind::Float(_) => {
            if ast.t_eq(suggestion, AstType::Float) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Float,
                    got: suggestion,
                })
            }
        }
        ExprKind::Bool(_) => {
            if ast.t_eq(suggestion, AstType::Bool) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Bool,
                    got: suggestion,
                })
            }
        }
        ExprKind::String(_) => {
            if ast.t_eq(suggestion, AstType::String) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::String,
                    got: suggestion,
                })
            }
        }
        ExprKind::Ident(s) => {
            if let Some(typ) = context.variables.get(s) {
                if ast.t_eq(suggestion, *typ) {
                    Ok(())
                } else {
                    error(ErrorKind::MismatchedTypes {
                        expected: typ.clone(),
                        got: suggestion,
                    })
                }
            } else {
                error(ErrorKind::NoSuchVariable(s.clone()))
            }
        }
        ExprKind::Call(fun_name, args) => {
            let Some(AstType::Function(args_types, ret_type)) =
                context.functions.get(fun_name).cloned()
            else {
                return error(ErrorKind::NoSuchFunction(fun_name.clone()));
            };
            for (arg, arg_typ) in args.zip(args_types) {
                type_check_expr(arg, ast, ast[arg_typ], context, errors)?;
            }
            if ast.t_eq(suggestion, ast[ret_type]) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: ast[ret_type].clone(),
                    got: suggestion,
                })
            }
        }
        ExprKind::Return(expr) => {
            type_check_expr(
                *expr,
                ast,
                context.return_type.clone().unwrap(),
                context,
                errors,
            )?;
            if ast.t_eq(suggestion, AstType::Never) || ast.t_eq(suggestion, AstType::Unit) {
                // TODO: write proper conversion predicate
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: context.return_type.clone().unwrap(),
                    got: suggestion,
                })
            }
        }
        ExprKind::If(cond, body, els) => {
            type_check_expr(*cond, ast, AstType::Bool, context, errors)?;
            for expr in *body {
                type_check_expr(expr, ast, AstType::Unit, context, errors)?;
            }
            if let Some(els) = els {
                for expr in *els {
                    type_check_expr(expr, ast, AstType::Unit, context, errors)?;
                }
            }
            if ast.t_eq(suggestion, AstType::Unit) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Unit,
                    got: suggestion,
                })
            }
        }
        ExprKind::While(cond, body) => {
            type_check_expr(*cond, ast, AstType::Bool, context, errors)?;
            for expr in *body {
                type_check_expr(expr, ast, AstType::Unit, context, errors)?;
            }
            if ast.t_eq(suggestion, AstType::Unit) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Unit,
                    got: suggestion,
                })
            }
        }
        ExprKind::New(id) => match suggestion {
            AstType::Pointer(type_id) => type_check_expr(*id, ast, ast[type_id], context, errors),
            AstType::Hole(_) => unreachable!(),
            _ => {
                let typ = type_derive_expr(*id, ast, context, errors)?;
                error(ErrorKind::MismatchedTypes {
                    expected: suggestion,
                    got: AstType::Pointer(ast.add_type(typ)),
                })
            }
        },
        ExprKind::Ref(id) => match suggestion {
            AstType::Pointer(type_id) => type_check_expr(*id, ast, ast[type_id], context, errors),
            AstType::Hole(_) => unreachable!(),
            _ => {
                let typ = type_derive_expr(*id, ast, context, errors)?;
                error(ErrorKind::MismatchedTypes {
                    expected: suggestion,
                    got: AstType::Pointer(ast.add_type(typ)),
                })
            }
        },
        ExprKind::Deref(id) => {
            let typ = ast.add_type(suggestion);
            type_check_expr(*id, ast, AstType::Pointer(typ), context, errors)
        }
        ExprKind::Let(_, _, _) => todo!(),
    }
}

pub fn type_check_function(
    fun_id: AstFunctionId,
    ast: &mut Ast,
    errors: &mut Errors,
) -> CompilerResult<()> {
    let fun = &ast[fun_id];

    let mut context = TypeContext::new();

    for (name, typ) in fun.args.iter() {
        context.add_variable(name.clone(), ast[typ]);
    }

    context.return_type = Some(ast[fun.ret_type]);

    for fun in &ast.functions {
        context.add_function(fun.name.clone(), fun.typ.clone());
    }

    for expr_id in fun.body {
        type_derive_expr(expr_id, ast, &mut context, errors)?;
    }
    Ok(())
}

pub fn type_check(ast: &mut Ast, errors: &mut Errors) -> CompilerResult<()> {
    for fun_id in 0..ast.functions.len() {
        type_check_function(AstFunctionId(fun_id), ast, errors)?;
    }
    Ok(())
}
