use std::ops::{Index, IndexMut};

use inkwell::{
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};

use crate::{
    backend::LLVMBackend,
    utils::{
        coutner::Countable,
        error::{CompilerError, CompilerResult, ErrorKind},
    },
};

pub mod builder;
pub mod irgen;

#[derive(Clone, Debug)]
pub struct Module {
    pub functions: Vec<IrFunction>,
}

impl Index<IrFunctionId> for Module {
    type Output = IrFunction;

    fn index(&self, index: IrFunctionId) -> &Self::Output {
        &self.functions[index.0]
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Generated(usize),
    Named(String),
    Constant(Constant),
}
impl Value {
    pub fn to_string(&self) -> Option<String> {
        match self {
            Value::Generated(n) => Some(format!("tmp_{}", n)),
            Value::Named(name) => Some(format!("{}", name)),
            _ => None,
        }
    }
}

impl Countable for Value {
    fn from_count(count: usize) -> Self {
        Value::Generated(count)
    }
}

pub struct OptionalValue(pub Option<Value>);

impl OptionalValue {
    pub fn unwrap(self) -> CompilerResult<Value> {
        match self.0 {
            Some(value) => Ok(value),
            None => Err(CompilerError::new(ErrorKind::ExpectedValue)),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Add {
        typ: IrType,
        lhs: Value,
        rhs: Value,
        result: Value,
    },
    Eq {
        typ: IrType,
        lhs: Value,
        rhs: Value,
        result: Value,
    },
    Not {
        value: Value,
        result: Value,
    },
    Store {
        ptr: Value,
        value_type: IrType,
        value: Value,
    },
    Load {
        typ: IrType,
        ptr: Value,
        res: Value,
    },
    StackAlloc {
        typ: IrType,
        result: Value,
    },
    HeapAlloc {
        typ: IrType,
        size: Value,
        result: Value,
    },
    Print(Value, IrType),
    Call {
        name: String,
        args: Vec<Value>,
        result: Value,
    },
    Move {
        cond: Option<(Value, BlockId)>,
        block: BlockId,
    },
    Return {
        value: Value,
    },
}

#[derive(Clone, Debug)]
pub struct IrFunction {
    pub name: String,
    pub args: Vec<(String, IrType)>,
    pub ret: IrType,
    pub body: Vec<Block>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IrFunctionId(pub usize);

impl Countable for IrFunctionId {
    fn from_count(count: usize) -> Self {
        IrFunctionId(count)
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub instructions: Vec<Instruction>,
}

impl Block {
    pub fn new() -> Block {
        Block {
            instructions: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub usize);

impl Countable for BlockId {
    fn from_count(count: usize) -> Self {
        BlockId(count)
    }
}

impl Index<BlockId> for IrFunction {
    type Output = Block;

    fn index(&self, index: BlockId) -> &Self::Output {
        &self.body[index.0]
    }
}

impl IndexMut<BlockId> for IrFunction {
    fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
        &mut self.body[index.0]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IrType {
    Int,
    Float,
    Bool,
    Pointer,
    Unit,
    Never,
}

impl IrType {
    pub fn to_string(&self) -> String {
        match self {
            IrType::Unit => "unit".to_string(),
            IrType::Never => "never".to_string(),
            IrType::Int => "int".to_string(),
            IrType::Float => "float".to_string(),
            IrType::Bool => "bool".to_string(),
            IrType::Pointer => "pointer".to_string(),
        }
    }

    pub fn size(&self) -> i64 {
        match self {
            IrType::Int => 8,
            IrType::Float => 8,
            IrType::Bool => 1,
            IrType::Pointer => 8,
            IrType::Unit => 0,
            IrType::Never => unreachable!(),
        }
    }

    pub fn to_llvm<'ctx>(&self, backend: &LLVMBackend<'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            IrType::Int => backend.context.i64_type().as_basic_type_enum(),
            IrType::Float => backend.context.f64_type().as_basic_type_enum(),
            IrType::Bool => backend.context.bool_type().as_basic_type_enum(),
            IrType::Pointer => backend
                .context
                .i64_type()
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            IrType::Unit => unreachable!(),
            IrType::Never => unreachable!(),
        }
    }
}
