use crate::{
    ast::{Ast, AstFunctionId, AstType, ExprId, ExprKind, TypeDef},
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
            if !types.iter().all(|typ| ast.t_eq(typ, last)) {
                return error(ErrorKind::SumIncompatibleTypes);
            }
            last.clone()
        }
        ExprKind::Eq(lhs, rhs) => {
            let lhs = type_derive_expr(lhs, ast, context, errors)?;
            type_check_expr(rhs, ast, &lhs, context, errors)?;
            AstType::Bool
        }
        ExprKind::Not(expr) => {
            type_check_expr(expr, ast, &AstType::Bool, context, errors)?;
            AstType::Bool
        }
        ExprKind::Set(ptr, value) => {
            let value_type = type_derive_expr(value, ast, context, errors)?;

            type_check_expr(ptr, ast, &value_type, context, errors)?;

            AstType::Unit
        }
        ExprKind::Print(expr) => {
            let _typ = type_derive_expr(expr, ast, context, errors)?;
            AstType::Unit
        }
        ExprKind::Integer(_) => AstType::Int,
        ExprKind::Float(_) => AstType::Float,
        ExprKind::Bool(_) => AstType::Bool,
        ExprKind::String(_) => ast.string_type(),
        ExprKind::Ident(var) => context
            .variables
            .get(&var)
            .cloned()
            .ok_or(CompilerError::new(ErrorKind::NoSuchVariable(var)))?,

        ExprKind::Call(fun, args) => {
            let Some(AstType::Function(args_types, ret_type)) =
                context.functions.get(&fun).cloned()
            else {
                let Some(TypeDef::Struct(strct)) = ast.type_defs.get(&fun).cloned() else {
                    return error(ErrorKind::NoSuchFunctionOrStruct(fun));
                };
                for (arg, field) in args.zip(strct.fields.iter()) {
                    let typ = ast[field.typ].clone();
                    type_check_expr(arg, ast, &typ, context, errors)?;
                }

                return Ok(AstType::Struct(strct.name.clone()));
            };
            for (arg, arg_type) in args.zip(args_types) {
                let typ = ast[arg_type].clone();
                type_check_expr(arg, ast, &typ, context, errors)?;
            }
            ast[ret_type].clone()
        }
        ExprKind::Return(expr) => {
            type_check_expr(
                expr,
                ast,
                &context.return_type.clone().unwrap(),
                context,
                errors,
            )?;
            AstType::Never
        }
        ExprKind::If(cond, then, els) => {
            type_check_expr(cond, ast, &AstType::Bool, context, errors)?;
            for expr in then {
                type_check_expr(expr, ast, &AstType::Unit, context, errors)?;
            }
            if let Some(els) = els {
                for expr in els {
                    type_check_expr(expr, ast, &AstType::Unit, context, errors)?;
                }
            }
            AstType::Unit
        }
        ExprKind::While(cond, body) => {
            type_check_expr(cond, ast, &AstType::Bool, context, errors)?;
            for expr in body {
                type_check_expr(expr, ast, &AstType::Unit, context, errors)?;
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
                ast[typ].clone()
            } else {
                return error(ErrorKind::UnexpectedType(format!("{:?}", typ)));
            }
        }
        ExprKind::Let(name, _typ, expr) => {
            let typ = type_derive_expr(expr, ast, context, errors)?;
            context.add_variable(name, typ.clone());
            AstType::Unit
        }
        ExprKind::FieldAccess(expr, field) => {
            let typ = type_derive_expr(expr, ast, context, errors)?;
            if let AstType::Struct(name) = typ {
                let TypeDef::Struct(strct) = ast.type_defs.get(&name).unwrap();
                let Some(field) = strct.fields.iter().find(|f| f.name == field) else {
                    return error(ErrorKind::NoSuchField(field));
                };
                ast[field.typ].clone()
            } else {
                return error(ErrorKind::UnexpectedType(format!("{:?}", typ)));
            }
        }
    };

    ast[expr.typ] = res.clone();

    Ok(res)
}

pub fn type_check_expr(
    expr_id: ExprId,
    ast: &mut Ast,
    suggestion: &AstType,
    context: &mut TypeContext,
    errors: &mut Errors,
) -> CompilerResult<()> {
    let expr = ast[expr_id].clone();
    ast[expr.typ] = suggestion.clone();
    match &expr.kind {
        ExprKind::Add(exprs) => {
            for expr in *exprs {
                type_check_expr(expr, ast, suggestion, context, errors)?;
            }
            Ok(())
        }
        ExprKind::Eq(lhs, rhs) => {
            let lhs_type = type_derive_expr(*lhs, ast, context, errors)?;
            type_check_expr(*rhs, ast, &lhs_type, context, errors)?;

            if ast.t_eq(suggestion, &AstType::Bool) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Bool,
                    got: suggestion.clone(),
                })
            }
        }
        ExprKind::Not(expr) => {
            type_check_expr(*expr, ast, &AstType::Bool, context, errors)?;
            if ast.t_eq(suggestion, &AstType::Bool) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Bool,
                    got: suggestion.clone(),
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
            if ast.t_eq(suggestion, &AstType::Unit) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Unit,
                    got: suggestion.clone(),
                })
            }
        }
        ExprKind::Integer(_) => {
            if ast.t_eq(suggestion, &AstType::Int) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Int,
                    got: suggestion.clone(),
                })
                .map_err(|e| e.message(format!("expr: {:?}", expr)))
            }
        }
        ExprKind::Float(_) => {
            if ast.t_eq(suggestion, &AstType::Float) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Float,
                    got: suggestion.clone(),
                })
            }
        }
        ExprKind::Bool(_) => {
            if ast.t_eq(suggestion, &AstType::Bool) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Bool,
                    got: suggestion.clone(),
                })
            }
        }
        ExprKind::String(_) => {
            if ast.t_eq(&suggestion, &ast.string_type()) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: ast.string_type(),
                    got: suggestion.clone(),
                })
            }
        }
        ExprKind::Ident(s) => {
            if let Some(typ) = context.variables.get(s) {
                if ast.t_eq(&suggestion, typ) {
                    Ok(())
                } else {
                    error(ErrorKind::MismatchedTypes {
                        expected: typ.clone(),
                        got: suggestion.clone(),
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
                let Some(TypeDef::Struct(strct)) = ast.type_defs.get(fun_name).cloned() else {
                    return error(ErrorKind::NoSuchFunctionOrStruct(fun_name.to_owned()));
                };
                for (arg, field) in args.zip(strct.fields.iter()) {
                    let typ = ast[field.typ].clone();
                    type_check_expr(arg, ast, &typ, context, errors)?;
                }

                return if ast.t_eq(&suggestion, &AstType::Struct(strct.name.clone())) {
                    Ok(())
                } else {
                    error(ErrorKind::MismatchedTypes {
                        expected: AstType::Struct(strct.name.clone()),
                        got: suggestion.clone(),
                    })
                };
            };
            for (arg, arg_typ) in args.zip(args_types) {
                let typ = ast[arg_typ].clone();
                type_check_expr(arg, ast, &typ, context, errors)?;
            }
            if ast.t_eq(&suggestion, &ast[ret_type]) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: ast[ret_type].clone(),
                    got: suggestion.clone(),
                })
            }
        }
        ExprKind::Return(expr) => {
            type_check_expr(
                *expr,
                ast,
                &context.return_type.clone().unwrap(),
                context,
                errors,
            )?;
            if ast.t_eq(suggestion, &context.return_type.as_ref().unwrap()) {
                // TODO: write proper conversion predicate
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: context.return_type.clone().unwrap(),
                    got: suggestion.clone(),
                })
            }
        }
        ExprKind::If(cond, body, els) => {
            type_check_expr(*cond, ast, &AstType::Bool, context, errors)?;
            for expr in *body {
                type_check_expr(expr, ast, &AstType::Unit, context, errors)?;
            }
            if let Some(els) = els {
                for expr in *els {
                    type_check_expr(expr, ast, &AstType::Unit, context, errors)?;
                }
            }
            if ast.t_eq(suggestion, &AstType::Unit) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Unit,
                    got: suggestion.clone(),
                })
            }
        }
        ExprKind::While(cond, body) => {
            type_check_expr(*cond, ast, &AstType::Bool, context, errors)?;
            for expr in *body {
                type_check_expr(expr, ast, &AstType::Unit, context, errors)?;
            }
            if ast.t_eq(suggestion, &AstType::Unit) {
                Ok(())
            } else {
                error(ErrorKind::MismatchedTypes {
                    expected: AstType::Unit,
                    got: suggestion.clone(),
                })
            }
        }
        ExprKind::New(id) => match suggestion {
            AstType::Pointer(type_id) => {
                type_check_expr(*id, ast, &ast[*type_id].clone(), context, errors)
            }
            AstType::Hole(_) => unreachable!(),
            _ => {
                let typ = type_derive_expr(*id, ast, context, errors)?;
                error(ErrorKind::MismatchedTypes {
                    expected: suggestion.clone(),
                    got: AstType::Pointer(ast.add_type(typ)),
                })
            }
        },
        ExprKind::Ref(id) => match suggestion {
            AstType::Pointer(type_id) => {
                type_check_expr(*id, ast, &ast[*type_id].clone(), context, errors)
            }
            AstType::Hole(_) => unreachable!(),
            _ => {
                let typ = type_derive_expr(*id, ast, context, errors)?;
                error(ErrorKind::MismatchedTypes {
                    expected: suggestion.clone(),
                    got: AstType::Pointer(ast.add_type(typ)),
                })
            }
        },
        ExprKind::Deref(id) => {
            let typ = ast.add_type(suggestion.clone());
            type_check_expr(*id, ast, &AstType::Pointer(typ), context, errors)
        }
        ExprKind::Let(_, _, _) => todo!(),
        ExprKind::FieldAccess(_, _) => todo!(),
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
        context.add_variable(name.clone(), ast[typ].clone());
    }

    context.return_type = Some(ast[fun.ret_type].clone());

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
