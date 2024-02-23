use std::fmt::Pointer;

use crate::{
    ir::{
        Constant, Instruction, IrEnum, IrEnumId, IrFunctionId, IrStruct, IrStructId, IrType,
        Module, Value,
    },
    utils::error::{CompilerResult, Errors},
};

#[derive(Debug, Clone, Copy)]
pub struct IdMap(usize);

impl IdMap {
    pub fn apply(self, enum_id: IrEnumId) -> IrStructId {
        IrStructId(self.0 + enum_id.0)
    }
}

fn lower_enum(enum_id: IrEnumId, module: &mut Module) -> CompilerResult<()> {
    let enum_info = &module.enums[enum_id.0];

    let fields = vec![IrType::Byte, IrType::IrArray(enum_info.size as usize - 1)];

    module.structs.push(IrStruct {
        name: enum_info.name.clone(),
        fields,
        size: enum_info.size,
    });
    Ok(())
}

fn map_instruction(
    instr: Instruction,
    id_map: IdMap,
    enums: &[IrEnum],
    errors: &mut Errors,
) -> CompilerResult<Vec<Instruction>> {
    match instr {
        Instruction::Variant {
            enum_id,
            variant,
            value,
            result,
        } => {
            let enum_info = &enums[enum_id.0];
            let struct_id = id_map.apply(enum_id);
            // rewrite variant as struct operations
            // alloc big struct
            let stack_alloc = Instruction::StackAlloc {
                typ: IrType::IrStruct(struct_id),
                result: result.clone(),
            };
            // get pointer to tag
            let tag_field_ptr = Value::Generated(todo!());
            let get_tag = Instruction::Field {
                struct_id,
                field: 0,
                value: result.clone(),
                result: tag_field_ptr.clone(),
            };

            // get pointer to payload
            let payload_field_ptr = Value::Generated(todo!());
            let get_payload = Instruction::Field {
                struct_id,
                field: 1,
                value: result,
                result: payload_field_ptr.clone(),
            };

            // cast pointer to payload to type of variant
            let casted_payload_field_ptr = Value::Generated(todo!());
            let typ = enum_info.variants[variant.0];
            let cast_payload = Instruction::Cast {
                cast_to_type: typ,
                value: payload_field_ptr,
                result: casted_payload_field_ptr.clone(),
            };

            // store tag
            let store_tag = Instruction::Store {
                ptr: tag_field_ptr.clone(),
                value_type: IrType::Byte,
                value: Value::Constant(Constant::Byte(variant.0 as _)),
            };

            // store value in pointer to payload
            let store_payload = Instruction::Store {
                ptr: casted_payload_field_ptr,
                value_type: typ,
                value,
            };

            Ok(vec![
                stack_alloc,
                get_tag,
                get_payload,
                cast_payload,
                store_tag,
                store_payload,
            ])
        }
        Instruction::Match {
            enum_id,
            value,
            branches,
            else_branch,
        } => todo!(),
        _ => Ok(vec![instr]),
    }
}

fn lower_function(
    fun_id: IrFunctionId,
    id_map: IdMap,
    module: &mut Module,
    errors: &mut Errors,
) -> CompilerResult<()> {
    let fun = &mut module.functions[fun_id.0];

    for block in &mut fun.body {
        let mut new_instrs = Vec::new();
        for instr in block.instructions.clone() {
            new_instrs.extend(map_instruction(instr, id_map, &module.enums, errors)?);
        }
        block.instructions = new_instrs;
    }

    Ok(())
}

pub fn lower_ir(module: &mut Module, errors: &mut Errors) -> CompilerResult<()> {
    // redefine enums as structs
    let id_map = IdMap(module.structs.len());
    for i in 0..module.enums.len() {
        lower_enum(IrEnumId(i), module)?;
    }

    for i in 0..module.functions.len() {
        // rewrite variant as struct operations and rewrite match as switch case
        lower_function(IrFunctionId(i), id_map, module, errors)?;
    }
    Ok(())
}
