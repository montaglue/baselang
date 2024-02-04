use std::collections::HashMap;

use crate::{
    ast::{Ast, AstFunction, Struct},
    utils::coutner::Counter,
};

use super::{
    Block, BlockId, Constant, Instruction, IrFunction, IrFunctionId, IrStruct, IrStructId, IrType,
    Module, Value,
};

pub struct Builder {
    current_function: Option<IrFunctionId>,
    current_block: Option<BlockId>,
    current_blocks: HashMap<IrFunctionId, BlockId>,
    functions: HashMap<String, IrFunctionId>,
    module: Module,
    value_counter: Counter<Value>,
    function_counter: Counter<IrFunctionId>,
    block_counters: HashMap<IrFunctionId, Counter<BlockId>>,
    struct_ids: HashMap<String, IrStructId>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            module: Module {
                structs: Vec::new(),
                functions: Vec::new(),
            },
            value_counter: Counter::new(),
            function_counter: Counter::new(),
            current_function: None,
            functions: HashMap::new(),
            current_block: None,
            current_blocks: HashMap::new(),
            block_counters: HashMap::new(),
            struct_ids: HashMap::new(),
        }
    }

    pub fn build_struct(&mut self, ast: &Ast, strct: Struct) {
        // TODO: allow struct be in any order

        let fields = strct
            .fields
            .into_iter()
            .map(|field| ast[field.typ].to_ir(self))
            .collect();

        let ir_struct = IrStruct {
            name: strct.name.clone(),
            fields,
        };

        let id = IrStructId(self.module.structs.len());
        self.module.structs.push(ir_struct);
        self.struct_ids.insert(strct.name, id);
    }

    pub fn get_struct(&self, name: &str) -> Option<IrStructId> {
        self.struct_ids.get(name).cloned()
    }

    pub fn current_block(&self) -> BlockId {
        self.current_block.unwrap()
    }

    pub fn add_instruction(&mut self, instr: Instruction) {
        let current_block = self.current_block.unwrap();
        self.current_function_mut()[current_block]
            .instructions
            .push(instr);
    }

    pub fn build_call(&mut self, name: String, args: Vec<Value>, res: Option<Value>) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::Call {
            name,
            args,
            result: res.clone(),
        };
        self.add_instruction(instruction);
        res
    }

    pub fn build_struct_construct(
        &mut self,
        struct_id: IrStructId,
        args: Vec<Value>,
        res: Option<Value>,
    ) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::Create {
            struct_id,
            args,
            result: res.clone(),
        };
        self.add_instruction(instruction);
        res
    }

    pub fn current_function(&self) -> &IrFunction {
        self.current_function
            .as_ref()
            .map(|id| &self.module.functions[id.0])
            .unwrap()
    }

    pub fn current_function_mut(&mut self) -> &mut IrFunction {
        self.current_function
            .as_mut()
            .map(|id| &mut self.module.functions[id.0])
            .unwrap()
    }

    pub fn set_current_function(&mut self, id: IrFunctionId) {
        self.current_function = Some(id);
    }

    pub fn build_add(&mut self, lhs: Value, rhs: Value, res: Option<Value>, typ: IrType) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::Add {
            typ,
            lhs,
            rhs,
            result: res.clone(),
        };
        self.add_instruction(instruction);
        res
    }

    pub fn build_print(&mut self, name: Value, typ: IrType) {
        let instruction = Instruction::Print(name, typ);
        self.add_instruction(instruction);
    }

    pub fn build_store(&mut self, value: Value, value_type: IrType, var: Option<Value>) -> Value {
        let ptr = var.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::Store {
            ptr: ptr.clone(),
            value_type,
            value,
        };
        self.add_instruction(instruction);
        ptr
    }

    pub fn build_load(&mut self, ptr: Value, typ: IrType, res: Option<Value>) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::Load {
            typ,
            res: res.clone(),
            ptr,
        };
        self.add_instruction(instruction);
        res
    }

    pub fn build_function_header(&mut self, fun: &AstFunction, ast: &Ast) -> IrFunctionId {
        let id = self.function_counter.next();
        let function = IrFunction {
            name: fun.name.clone(),
            args: fun
                .args
                .iter()
                .map(|(name, typ)| (name.clone(), ast[typ].to_ir(self)))
                .collect(),
            ret: ast[fun.ret_type].to_ir(self),
            body: Vec::new(),
        };
        self.module.functions.push(function);
        self.block_counters.insert(id, Counter::new());
        self.functions.insert(fun.name.clone(), id);
        id
    }

    pub fn build_block(&mut self) -> BlockId {
        let fun_id = self.current_function.unwrap();
        let id = self.block_counters.get_mut(&fun_id).unwrap().next();
        self.current_function_mut().body.push(Block::new());
        id
    }

    pub fn set_current_block(&mut self, id: BlockId) {
        self.current_block = Some(id);
        let current_function = self.current_function.unwrap();
        self.current_blocks.insert(current_function, id);
    }

    pub fn build_return(&mut self, value: Value) {
        let instruction = Instruction::Return { value };
        self.add_instruction(instruction);
    }

    pub fn build_move(&mut self, block: BlockId, cond: Option<Value>, alt_branch: Option<BlockId>) {
        let cond = cond.map(|cond| (cond, alt_branch.unwrap()));
        let instruction = Instruction::Move { cond, block };
        self.add_instruction(instruction);
    }

    pub fn build_eq(&mut self, lhs: Value, rhs: Value, res: Option<Value>, typ: IrType) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::Eq {
            typ,
            lhs,
            rhs,
            result: res.clone(),
        };
        self.add_instruction(instruction);
        res
    }

    pub fn build(mut self) -> Module {
        for function in &mut self.module.functions {
            for block in &mut function.body {
                if block.instructions.is_empty() {
                    block.instructions.push(Instruction::Return {
                        value: Value::Constant(Constant::Int(0)),
                    });
                }
            }
        }

        self.module
    }

    pub fn build_not(&mut self, value: Value, res: Option<Value>) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::Not {
            value,
            result: res.clone(),
        };
        self.add_instruction(instruction);
        res
    }

    pub fn build_heap_alloc(&mut self, typ: IrType, size: Value, res: Option<Value>) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::HeapAlloc {
            typ,
            size,
            result: res.clone(),
        };
        self.add_instruction(instruction);
        res
    }

    pub fn build_stack_alloc(&mut self, typ: IrType, res: Option<Value>) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::StackAlloc {
            typ,
            result: res.clone(),
        };
        self.add_instruction(instruction);
        res
    }

    pub fn build_field_access(
        &mut self,
        value: Value,
        field: usize,
        struct_id: IrStructId,
        res: Option<Value>,
    ) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::Field {
            value,
            field,
            struct_id,
            result: res.clone(),
        };
        self.add_instruction(instruction);
        res
    }
}
