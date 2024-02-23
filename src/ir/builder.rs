use std::collections::HashMap;

use crate::{
    ast::{Ast, AstFunction, Enum, Struct},
    utils::coutner::Counter,
};

use super::{
    Block, BlockId, Constant, Instruction, IrEnum, IrEnumId, IrFunction, IrFunctionId, IrStruct,
    IrStructId, IrType, Module, Value, VariantId,
};

pub struct Builder {
    current_function: Option<IrFunctionId>,
    current_block: Option<BlockId>,
    current_blocks: HashMap<IrFunctionId, BlockId>,
    functions: HashMap<String, IrFunctionId>,
    pub module: Module,
    value_counter: Counter<Value>,
    function_counter: Counter<IrFunctionId>,
    block_counters: HashMap<IrFunctionId, Counter<BlockId>>,
    struct_ids: HashMap<String, IrStructId>,
    enum_ids: HashMap<String, IrEnumId>,
    variants: HashMap<(IrEnumId, String), VariantId>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            module: Module {
                structs: Vec::new(),
                functions: Vec::new(),
                enums: Vec::new(),
            },
            value_counter: Counter::new(),
            function_counter: Counter::new(),
            current_function: None,
            functions: HashMap::new(),
            current_block: None,
            current_blocks: HashMap::new(),
            block_counters: HashMap::new(),
            struct_ids: HashMap::new(),
            enum_ids: HashMap::new(),
            variants: HashMap::new(),
        }
    }

    pub fn build_enum(&mut self, ast: &Ast, enm: Enum) -> IrEnumId {
        // for
        // let variants: HashMap<String, IrType> = enm
        //     .variants
        //     .into_iter()
        //     .map(|(var_name, var_type)| (var_name, ast[var_type].to_ir_lazy(self)))
        //     .collect();
        let id = self.get_enum_or_insert(&enm.name);

        let mut variants: Vec<IrType> = vec![];

        for (i, (var_name, var_type)) in enm.variants.iter().enumerate() {
            let typ = ast[*var_type].to_ir(self);
            variants.push(typ);
            self.variants.insert((id, var_name.clone()), VariantId(i));
        }

        let mut size = 0;

        for typ in variants.iter() {
            size = size.max(typ.size_in_module(&self.module));
        }
        size += 1;

        let enum_ref = &mut self.module.enums[id.0];
        enum_ref.variants = variants;
        enum_ref.size = size;

        id
    }

    pub fn build_struct(&mut self, ast: &Ast, strct: Struct) {
        let id = self.get_struct_or_insert(&strct.name);

        let fields: Vec<IrType> = strct
            .fields
            .into_iter()
            .map(|field| ast[field.typ].to_ir(self))
            .collect();

        let mut size = 0;

        for typ in fields.iter() {
            size += typ.size_in_module(&self.module);
        }

        let struct_ref = &mut self.module.structs[id.0];
        struct_ref.fields = fields;
        struct_ref.size = size;
    }

    pub fn get_struct(&self, name: &str) -> Option<IrStructId> {
        self.struct_ids.get(name).cloned()
    }

    pub fn get_struct_or_insert(&mut self, name: &str) -> IrStructId {
        if let Some(id) = self.struct_ids.get(name).cloned() {
            return id;
        }

        let id = IrStructId(self.module.structs.len());
        self.module.structs.push(IrStruct {
            name: name.to_string(),
            fields: Vec::new(),
            size: 0,
        });
        self.struct_ids.insert(name.to_string(), id);
        id
    }

    pub fn get_enum_or_insert(&mut self, name: &str) -> IrEnumId {
        if let Some(id) = self.enum_ids.get(name).cloned() {
            return id;
        }

        let id = IrEnumId(self.module.enums.len());
        self.module.enums.push(IrEnum {
            name: name.to_string(),
            variants: Vec::new(),
            size: 0,
        });
        self.enum_ids.insert(name.to_string(), id);
        id
    }

    pub fn get_enum(&self, name: &str) -> Option<IrEnumId> {
        self.enum_ids.get(name).cloned()
    }

    pub fn get_variant(&self, enum_id: IrEnumId, name: &str) -> Option<VariantId> {
        self.variants.get(&(enum_id, name.to_string())).cloned()
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

    pub fn build_enum_variant(
        &mut self,
        enum_id: IrEnumId,
        variant_id: VariantId,
        arg: Value,
        res: Option<Value>,
    ) -> Value {
        let res = res.unwrap_or_else(|| self.value_counter.next());
        let instruction = Instruction::Variant {
            enum_id,
            variant: variant_id,
            value: arg,
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
