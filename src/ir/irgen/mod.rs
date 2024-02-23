pub mod lower_enum;

use crate::{
    ast::{Ast, AstFunctionId, AstType, ExprId, ExprKind, TypeDef},
    ir::{builder::Builder, Constant},
    utils::error::{error, CompilerResult, ErrorKind, Errors},
};

use super::{IrType, Module, OptionalValue, Value};

pub fn generate_ir(ast: &Ast, errors: &mut Errors) -> CompilerResult<Module> {
    println!("module generation begin");
    let mut module = build_ir(ast, errors)?;

    println!("module lowering begin");

    lower_enum::lower_ir(&mut module, errors)?;

    Ok(module)
}

fn build_ir(ast: &Ast, errors: &mut Errors) -> CompilerResult<Module> {
    let mut builder = Builder::new();

    for (name, type_def) in &ast.type_defs {
        println!("generated type {}", name);
        match type_def {
            TypeDef::Struct(strct) => {
                builder.build_struct(ast, strct.clone());
            }
            TypeDef::Enum(enm) => {
                builder.build_enum(ast, enm.clone());
            }
        }
    }

    for i in 0..ast.functions.len() {
        generate_function(AstFunctionId(i), ast, &mut builder, errors)?;
    }

    Ok(builder.build())
}

fn generate_function(
    fun: AstFunctionId,
    ast: &Ast,
    builder: &mut Builder,
    errors: &mut Errors,
) -> CompilerResult<()> {
    let fun = &ast[fun];
    let f_id = builder.build_function_header(fun, ast);
    builder.set_current_function(f_id);

    let entry = builder.build_block();

    builder.set_current_block(entry);

    for expr_id in fun.body {
        generate_expr(expr_id, ast, builder, errors)?;
    }

    Ok(())
}

fn generate_ref(
    expr_id: ExprId,
    ast: &Ast,
    builder: &mut Builder,
    errors: &mut Errors,
) -> CompilerResult<Value> {
    let typ = ast.get_type(expr_id);

    if typ.to_ir(builder) == IrType::Pointer {
        let value = generate_expr(expr_id, ast, builder, errors)?.unwrap()?;
        return Ok(value);
    }

    if let ExprKind::Deref(expr) = &ast[expr_id].kind {
        let value = generate_expr(*expr, ast, builder, errors)?.unwrap()?;
        return Ok(value);
    }

    if let ExprKind::Ident(ident) = &ast[expr_id].kind {
        return Ok(Value::Named(ident.clone()));
    }
    error(ErrorKind::CantMakeReferenceFrom(ast[expr_id].clone()))
}

fn generate_expr(
    expr_id: ExprId,
    ast: &Ast,
    builder: &mut Builder,
    errors: &mut Errors,
) -> CompilerResult<OptionalValue> {
    let expr = &ast[expr_id];

    let result: Option<Value> = match &expr.kind {
        ExprKind::Add(exprs) => {
            let mut result = None;
            let mut result_type: Option<AstType> = None;
            for expr_id in *exprs {
                if let (Some(lhs), Some(typ)) = (result, &result_type) {
                    let rhs = generate_expr(expr_id, ast, builder, errors)?.unwrap()?;
                    result = Some(builder.build_add(lhs, rhs, None, typ.to_ir(builder)));
                } else {
                    result = Some(generate_expr(expr_id, ast, builder, errors)?.unwrap()?);
                    result_type = Some(ast[expr.typ].clone());
                }
            }
            result
        }
        ExprKind::Eq(lhs, rhs) => {
            let typ = ast.get_type(*lhs).to_ir(builder);
            let lhs = generate_expr(*lhs, ast, builder, errors)?.unwrap()?;
            let rhs = generate_expr(*rhs, ast, builder, errors)?.unwrap()?;
            Some(builder.build_eq(lhs, rhs, None, typ))
        }
        ExprKind::Set(lhs, rhs) => {
            let ptr = generate_ref(*lhs, ast, builder, errors)?;
            let value = generate_expr(*rhs, ast, builder, errors)?.unwrap()?;
            let typ = ast.get_type(*rhs).to_ir(builder);
            builder.build_store(value, typ, Some(ptr));
            None
        }
        ExprKind::Print(expr) => {
            let value = generate_expr(*expr, ast, builder, errors)?.unwrap()?;
            let typ = ast.get_type(*expr).to_ir(builder);
            builder.build_print(value, typ);
            None
        }
        ExprKind::Integer(i) => Some(Value::Constant(Constant::Int(*i))),
        ExprKind::Ident(ident) => {
            let typ = ast.get_type(expr_id).to_ir(builder);
            let ptr = builder.build_load(Value::Named(ident.clone()), typ, None);
            Some(ptr)
        }
        ExprKind::Call(fun_name, args) => {
            let args = args
                .map(|expr_id| Ok(generate_expr(expr_id, ast, builder, errors)?.unwrap()?))
                .collect::<CompilerResult<Vec<_>>>()?;
            Some(builder.build_call(fun_name.clone(), args, None))
        }
        ExprKind::Return(expr) => {
            let value = generate_expr(*expr, ast, builder, errors)?.unwrap()?;
            builder.build_return(value);
            None
        }
        ExprKind::If(cond, then_exprs, else_exprs) => {
            let cond = generate_expr(*cond, ast, builder, errors)?.unwrap()?;

            let then = builder.build_block();
            let els = builder.build_block();
            let merge = builder.build_block();
            builder.build_move(then, Some(cond), Some(els));

            builder.set_current_block(then);
            for expr in *then_exprs {
                generate_expr(expr, ast, builder, errors)?;
            }
            builder.build_move(merge, None, None);

            builder.set_current_block(els);

            if let Some(else_exprs) = else_exprs {
                for expr in *else_exprs {
                    generate_expr(expr, ast, builder, errors)?;
                }

                builder.build_move(merge, None, None);
            }

            builder.set_current_block(merge);
            None
        }
        ExprKind::While(cond, body) => {
            let cond_block = builder.build_block();
            let body_block = builder.build_block();
            let merge_block = builder.build_block();

            builder.build_move(cond_block, None, None);

            builder.set_current_block(cond_block);
            let cond = generate_expr(*cond, ast, builder, errors)?.unwrap()?;
            builder.build_move(body_block, Some(cond), Some(merge_block));

            builder.set_current_block(body_block);
            for expr in *body {
                generate_expr(expr, ast, builder, errors)?;
            }
            builder.build_move(cond_block, None, None);

            None
        }
        ExprKind::Not(expr) => {
            let value = generate_expr(*expr, ast, builder, errors)?.unwrap()?;
            Some(builder.build_not(value, None))
        }
        ExprKind::Float(f) => {
            let value = Value::Constant(Constant::Float(*f));
            Some(value)
        }
        ExprKind::Bool(b) => {
            let value = Value::Constant(Constant::Bool(*b));
            Some(value)
        }
        ExprKind::String(s) => {
            let value = Value::Constant(Constant::String(s.clone()));

            Some(value)
        }
        ExprKind::New(expr) => {
            let value = generate_expr(*expr, ast, builder, errors)?.unwrap()?;
            let typ = ast.get_type(*expr).to_ir(builder);
            let alloc =
                builder.build_heap_alloc(typ, Value::Constant(Constant::Int(typ.size())), None);
            Some(builder.build_store(value, typ, Some(alloc)))
        }
        ExprKind::Ref(expr) => {
            let value = generate_ref(*expr, ast, builder, errors)?;
            Some(value)
        }
        ExprKind::Deref(ptr) => {
            let ptr_value = generate_expr(*ptr, ast, builder, errors)?.unwrap()?;
            let typ = ast.get_type(*ptr).to_ir(builder);
            Some(builder.build_load(ptr_value, typ, None))
        }
        ExprKind::Let(name, typ, expr) => {
            let value = generate_expr(*expr, ast, builder, errors)?.unwrap()?;
            let typ = ast.get_type(*expr).to_ir(builder);
            builder.build_stack_alloc(typ, Some(Value::Named(name.clone())));

            builder.build_store(value, typ, Some(Value::Named(name.clone())));
            None
        }
        ExprKind::FieldAccess(expr, field) => {
            let value = generate_expr(*expr, ast, builder, errors)?.unwrap()?;
            let AstType::Struct(ast_struct) = ast.get_type(*expr) else {
                unreachable!()
            };
            let TypeDef::Struct(struct_info) = &ast.type_defs[ast_struct] else {
                unreachable!()
            };
            let field_id = struct_info
                .fields
                .iter()
                .position(|f| &f.name == field)
                .unwrap();
            let struct_id = builder.get_struct(ast_struct).unwrap();

            let value = builder.build_field_access(value, field_id, struct_id, None);

            Some(value)
        }
        ExprKind::StructConstructor(fun_name, args) => {
            let args = args
                .map(|expr_id| Ok(generate_expr(expr_id, ast, builder, errors)?.unwrap()?))
                .collect::<CompilerResult<Vec<_>>>()?;

            let struct_id = builder.get_struct(&fun_name).unwrap();
            Some(builder.build_struct_construct(struct_id, args, None))
        }
        ExprKind::EnumConstructor(constructor, enm, arg) => {
            let value = generate_expr(*arg, ast, builder, errors)?.unwrap()?;
            let enum_id = builder.get_enum(enm).unwrap();
            let variant_id = builder.get_variant(enum_id, constructor).unwrap();
            let result = builder.build_enum_variant(enum_id, variant_id, value, None);
            Some(result)
        }
    };
    Ok(OptionalValue(result))
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, utils::error::Errors};

    #[test]
    fn set_indent() {
        let mut parser = Parser::new(include_str!("../../../examples/set_ident.base"));
        let mut errors = Errors::new();

        let ast = parser.parse(&mut errors).unwrap();
        let ir = super::build_ir(&ast, &mut errors).unwrap();
        println!("{:#?}", ir);
    }

    #[test]
    fn test_functions() {
        let mut parser = Parser::new(include_str!("../../../examples/functions.base"));
        let mut errors = Errors::new();

        let ast = parser.parse(&mut errors).unwrap();
        let ir = super::build_ir(&ast, &mut errors).unwrap();
        println!("{:#?}", ir);
    }

    #[test]
    fn test_if() {
        let mut parser = Parser::new(include_str!("../../../examples/if.base"));
        let mut errors = Errors::new();

        let ast = parser.parse(&mut errors).unwrap();
        println!("{:?}", ast);
        let ir = super::build_ir(&ast, &mut errors).unwrap();
        println!("{:#?}", ir);
    }
}
