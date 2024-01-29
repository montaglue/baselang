use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    context::Context,
    module::{Linkage, Module},
    types::BasicType,
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue, PointerValue},
    AddressSpace,
};

use crate::{
    ir::{self, BlockId, Constant, IrFunctionId, IrType, Value},
    utils::error::{error, CompilerError, CompilerResult, ErrorKind, Errors},
};

pub struct LLVMBackend<'ctx> {
    pub context: &'ctx inkwell::context::Context,
    pub module: inkwell::module::Module<'ctx>,
    pub builder: inkwell::builder::Builder<'ctx>,
    pub block_map: HashMap<(IrFunctionId, BlockId), BasicBlock<'ctx>>,
}

impl<'ctx> LLVMBackend<'ctx> {
    pub fn new(context: &'ctx Context) -> LLVMBackend<'ctx> {
        LLVMBackend {
            context,
            module: context.create_module("main"),
            builder: context.create_builder(),
            block_map: HashMap::new(),
        }
    }
}

fn printf_function<'ctx>(backend: &LLVMBackend<'ctx>) -> FunctionValue<'ctx> {
    backend.module.get_function("printf").unwrap_or_else(|| {
        let printf_t = backend.context.i32_type().fn_type(
            &[backend
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()],
            true,
        );

        let printf = backend
            .module
            .add_function("printf", printf_t, Some(Linkage::External));
        printf.set_call_conventions(0);

        printf
    })
}

fn make_global_constant<'ctx>(
    backend: &LLVMBackend<'ctx>,
    name: String,
    value: String,
) -> CompilerResult<PointerValue<'ctx>> {
    let Some(global) = backend.module.get_global(&name) else {
        return backend
            .builder
            .build_global_string_ptr(&value, &name)
            .map(GlobalValue::as_pointer_value)
            .map_err(CompilerError::from_llvm_builder);
    };
    Ok(global.as_pointer_value())
}

fn printf<'ctx>(
    arg: BasicValueEnum,
    typ: &IrType,
    backend: &LLVMBackend<'ctx>,
    _errors: &mut Errors,
) -> CompilerResult<()> {
    let printf = printf_function(backend);

    let map: HashMap<_, _> = vec![
        (IrType::Bool, "%d"),
        (IrType::Int, "%d"),
        (IrType::Float, "%f"),
        (IrType::Pointer, "%s"),
    ]
    .into_iter()
    .collect();

    let ptr = make_global_constant(
        backend,
        format!("print_{}", typ.to_string()),
        format!("{}\n", map[&typ]),
    )?;

    backend
        .builder
        .build_call(printf, &[ptr.into(), arg.into()], "call")
        .map_err(CompilerError::from_llvm_builder)?;
    Ok(())
}

fn mallock<'ctx>(
    size: BasicValueEnum<'ctx>,
    backend: &LLVMBackend<'ctx>,
    _errors: &mut Errors,
    value: &Value,
) -> CompilerResult<PointerValue<'ctx>> {
    let malloc = backend.module.get_function("malloc").unwrap_or_else(|| {
        let malloc_t = backend
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[backend.context.i64_type().into()], false);

        let malloc = backend
            .module
            .add_function("malloc", malloc_t, Some(Linkage::External));
        malloc.set_call_conventions(0);

        malloc
    });

    let ptr = backend
        .builder
        .build_call(malloc, &[size.into()], &value.to_string().unwrap())
        .map_err(CompilerError::from_llvm_builder)?
        .try_as_basic_value()
        .left()
        .unwrap();

    Ok(ptr.into_pointer_value())
}

fn normalize<'ctx>(
    value: &Value,
    values: &HashMap<String, BasicValueEnum<'ctx>>,
    backend: &LLVMBackend<'ctx>,
) -> CompilerResult<BasicValueEnum<'ctx>> {
    Ok(match value {
        Value::Generated(_) | Value::Named(_) => values[&value.to_string().unwrap()].clone(),

        Value::Constant(Constant::Int(i)) => backend
            .context
            .i64_type()
            .const_int(*i as u64, false)
            .into(),
        Value::Constant(Constant::Bool(b)) => backend
            .context
            .bool_type()
            .const_int(*b as u64, false)
            .into(),
        Value::Constant(Constant::Float(f)) => backend.context.f64_type().const_float(*f).into(),
        Value::Constant(Constant::String(_s)) => {
            todo!()
        }
    })
}

fn build_function_header<'ctx>(
    fun_id: IrFunctionId,
    ir: &ir::Module,
    backend: &mut LLVMBackend<'ctx>,
) {
    let ir_fun = &ir[fun_id];

    let param_types = ir_fun
        .args
        .iter()
        .map(|arg| arg.1.to_llvm(backend).into())
        .collect::<Vec<_>>();
    let fun_value = backend.module.add_function(
        &ir_fun.name,
        backend.context.i64_type().fn_type(&param_types, false),
        None,
    );

    for (i, _) in ir_fun.body.iter().enumerate() {
        let basic_block = backend
            .context
            .append_basic_block(fun_value, &format!("block_{}", i));
        backend.block_map.insert((fun_id, BlockId(i)), basic_block);
    }
}

fn build_add<'ctx>(
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
    result: &str,
    typ: &IrType,
    backend: &LLVMBackend<'ctx>,
) -> CompilerResult<BasicValueEnum<'ctx>> {
    Ok(match typ {
        IrType::Int => backend
            .builder
            .build_int_add(lhs.into_int_value(), rhs.into_int_value(), result)
            .map_err(CompilerError::from_llvm_builder)?
            .into(),
        IrType::Float => backend
            .builder
            .build_float_add(lhs.into_float_value(), rhs.into_float_value(), result)
            .map_err(CompilerError::from_llvm_builder)?
            .into(),
        _ => {
            unreachable!()
        }
    })
}

fn build_eq<'ctx>(
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
    result: &str,
    typ: &IrType,
    backend: &LLVMBackend<'ctx>,
) -> CompilerResult<BasicValueEnum<'ctx>> {
    Ok(match typ {
        IrType::Int => backend
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                lhs.into_int_value(),
                rhs.into_int_value(),
                result,
            )
            .map_err(CompilerError::from_llvm_builder)?
            .into(),
        IrType::Float => backend
            .builder
            .build_float_compare(
                inkwell::FloatPredicate::OEQ,
                lhs.into_float_value(),
                rhs.into_float_value(),
                result,
            )
            .map_err(CompilerError::from_llvm_builder)?
            .into(),
        IrType::Bool => backend
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                lhs.into_int_value(),
                rhs.into_int_value(),
                result,
            )
            .map_err(CompilerError::from_llvm_builder)?
            .into(),

        _ => {
            unreachable!()
        }
    })
}

fn build_function<'ctx>(
    fun_id: IrFunctionId,
    ir: &ir::Module,
    backend: &LLVMBackend<'ctx>,
    errors: &mut Errors,
) -> CompilerResult<()> {
    let ir_fun = &ir[fun_id];
    let Some(fun) = backend.module.get_function(&ir_fun.name) else {
        return error(ErrorKind::NoSuchFunction(ir_fun.name.clone()));
    };

    backend
        .builder
        .position_at_end(backend.block_map[&(fun_id, BlockId(0))]);

    let mut variables: HashMap<String, BasicValueEnum<'_>> = HashMap::new();

    for (i, arg) in fun.get_param_iter().enumerate() {
        let ptr_arg = backend
            .builder
            .build_alloca(ir_fun.args[i].1.to_llvm(backend), &ir_fun.args[i].0)
            .map_err(CompilerError::from_llvm_builder)?;

        backend
            .builder
            .build_store(ptr_arg, arg)
            .map_err(CompilerError::from_llvm_builder)?;

        variables.insert(ir_fun.args[i].0.clone(), ptr_arg.as_basic_value_enum());
    }

    for (i, block) in ir_fun.body.iter().enumerate() {
        backend
            .builder
            .position_at_end(backend.block_map[&(fun_id, BlockId(i))]);
        for inst in &block.instructions {
            match inst {
                ir::Instruction::Add {
                    lhs,
                    rhs,
                    result,
                    typ,
                } => {
                    let lhs = normalize(&lhs, &variables, &backend)?;
                    let rhs = normalize(&rhs, &variables, &backend)?;

                    let value = build_add(lhs, rhs, &result.to_string().unwrap(), typ, backend)?;

                    variables.insert(result.to_string().unwrap(), value);
                }
                ir::Instruction::Eq {
                    lhs,
                    rhs,
                    result,
                    typ,
                } => {
                    let lhs = normalize(&lhs, &variables, &backend)?;
                    let rhs = normalize(&rhs, &variables, &backend)?;
                    let value = build_eq(lhs, rhs, &result.to_string().unwrap(), typ, backend)?;

                    variables.insert(result.to_string().unwrap(), value);
                }
                ir::Instruction::Store {
                    ptr,
                    value_type,
                    value,
                } => {
                    let name = ptr.to_string().unwrap();
                    let ptr = if let Some(ptr) = variables.get(&name) {
                        *ptr
                    } else {
                        // unreachable!("{}", name)
                        let ptr = backend
                            .builder
                            .build_alloca(value_type.to_llvm(backend), &name.to_string())
                            .map_err(CompilerError::from_llvm_builder)?
                            .as_basic_value_enum();
                        variables.insert(name.to_owned(), ptr);
                        ptr
                    };
                    let value = normalize(&value, &variables, &backend)?;
                    backend
                        .builder
                        .build_store(ptr.into_pointer_value(), value)
                        .map_err(CompilerError::from_llvm_builder)?;
                }
                ir::Instruction::Load { ptr, res, typ } => {
                    let ptr = variables[&ptr.to_string().unwrap()];
                    let value = backend
                        .builder
                        .build_load(
                            typ.to_llvm(backend),
                            ptr.into_pointer_value(),
                            &res.to_string().unwrap(),
                        )
                        .map_err(CompilerError::from_llvm_builder)?;

                    variables.insert(res.to_string().unwrap(), value);
                }
                ir::Instruction::Print(value, typ) => {
                    let value = normalize(&value, &variables, &backend)?;
                    printf(value, typ, &backend, errors)?;
                }
                ir::Instruction::Call { name, args, result } => {
                    let args = args
                        .iter()
                        .map(|arg| Ok(normalize(arg, &variables, &backend)?.into()))
                        .collect::<CompilerResult<Vec<_>>>()?;

                    let fun = backend.module.get_function(name).ok_or_else(|| {
                        CompilerError::new(ErrorKind::NoSuchFunction(name.clone()))
                    })?;

                    let call_result = backend
                        .builder
                        .build_call(fun, &args, &result.to_string().unwrap())
                        .map_err(CompilerError::from_llvm_builder)?
                        .try_as_basic_value()
                        .left()
                        .unwrap(); // TODO: handle unit return type

                    let Value::Generated(result) = result else {
                        panic!("Unable to store to unamed value");
                    };
                    variables.insert(result.to_string(), call_result);
                }
                ir::Instruction::Return { value } => {
                    let value = normalize(&value, &variables, &backend)?;
                    backend
                        .builder
                        .build_return(Some(&value))
                        .map_err(CompilerError::from_llvm_builder)?;
                }
                ir::Instruction::Move { cond, block } => {
                    if let Some((cond, alt_branch)) = cond {
                        let cond = normalize(cond, &variables, &backend)?.into_int_value();
                        backend
                            .builder
                            .build_conditional_branch(
                                cond,
                                backend.block_map[&(fun_id, *block)],
                                backend.block_map[&(fun_id, *alt_branch)],
                            )
                            .map_err(CompilerError::from_llvm_builder)?;
                    } else {
                        backend
                            .builder
                            .build_unconditional_branch(backend.block_map[&(fun_id, *block)])
                            .map_err(CompilerError::from_llvm_builder)?;
                    }
                }
                ir::Instruction::Not { value, result } => {
                    let value = normalize(&value, &variables, &backend)?.into_int_value();
                    let value = backend
                        .builder
                        .build_not(value, &result.to_string().unwrap())
                        .map_err(CompilerError::from_llvm_builder)?;

                    variables.insert(result.to_string().unwrap(), value.into());
                }
                ir::Instruction::HeapAlloc { typ, size, result } => {
                    let size = normalize(&size, &variables, &backend)?;
                    let ptr = mallock(size, &backend, errors, result)?;
                    let ptr = backend
                        .builder
                        .build_pointer_cast(
                            ptr,
                            typ.to_llvm(backend).ptr_type(AddressSpace::default()),
                            "cast",
                        )
                        .map_err(CompilerError::from_llvm_builder)?;

                    variables.insert(result.to_string().unwrap(), ptr.into());
                }
                ir::Instruction::StackAlloc { typ, result } => {
                    let name = result.to_string().unwrap();
                    let ptr = backend
                        .builder
                        .build_alloca(typ.to_llvm(backend), &name)
                        .map_err(CompilerError::from_llvm_builder)?
                        .as_basic_value_enum();
                    variables.insert(name, ptr);
                }
            }
        }
    }

    Ok(())
}

pub fn build<'ctx>(
    ir: &ir::Module,
    context: &'ctx Context,
    errors: &mut Errors,
) -> CompilerResult<Module<'ctx>> {
    let mut backend = LLVMBackend::new(context);

    for fun_id in 0..ir.functions.len() {
        build_function_header(IrFunctionId(fun_id), &ir, &mut backend);
    }

    for fun_id in 0..ir.functions.len() {
        build_function(IrFunctionId(fun_id), &ir, &backend, errors)?;
    }

    Ok(backend.module)
}
