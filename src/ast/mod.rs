use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use crate::{
    ir::{builder::Builder, IrType},
    utils::coutner::Countable,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub functions: Vec<AstFunction>,
    pub exprs: Vec<Expr>,
    pub types: Vec<AstType>,
    pub type_defs: HashMap<String, TypeDef>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            exprs: Vec::new(),
            types: vec![AstType::Char, AstType::Pointer(AstTypeId(0))],
            type_defs: HashMap::new(),
        }
    }

    pub fn string_type(&self) -> AstType {
        AstType::Pointer(AstTypeId(0))
    }

    pub fn get_type(&self, expr_id: ExprId) -> &AstType {
        let type_id = self[expr_id].typ;
        &self[type_id]
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprId {
        let id = ExprId(self.exprs.len());
        self.exprs.push(expr);
        id
    }

    pub fn add_exprs(&mut self, expresions: Vec<Expr>) -> Exprs {
        let exprs = Exprs(ExprId(self.exprs.len()), expresions.len());
        self.exprs.extend(expresions);
        exprs
    }

    pub fn add_type(&mut self, typ: AstType) -> AstTypeId {
        if let AstType::Hole(i) = typ {
            return AstTypeId(i);
        }
        let id = AstTypeId(self.types.len());
        self.types.push(typ);
        id
    }

    pub fn add_types(&mut self, types_values: Vec<AstType>) -> AstTypes {
        let types = AstTypes(AstTypeId(self.types.len()), types_values.len());
        self.types.extend(types_values);
        types
    }

    pub fn add_function(&mut self, function: AstFunction) {
        self.functions.push(function);
    }

    pub fn new_type(&mut self) -> AstTypeId {
        let id = AstTypeId(self.types.len());
        self.types.push(AstType::Hole(id.0));
        id
    }

    pub fn add_struct(&mut self, strct: Struct) {
        self.type_defs
            .insert(strct.name.clone(), TypeDef::Struct(strct));
    }

    pub fn t_eq(&self, lhs: &AstType, rhs: &AstType) -> bool {
        match (lhs, rhs) {
            (AstType::Hole(_), _) => true,
            (_, AstType::Hole(_)) => true,
            (&AstType::Unit, &AstType::Unit) => true,
            (AstType::Never, AstType::Never) => true,
            (AstType::Int, AstType::Int) => true,
            (AstType::Float, AstType::Float) => true,
            (AstType::Bool, AstType::Bool) => true,
            (AstType::Char, AstType::Char) => true,
            (AstType::Pointer(lhs), AstType::Pointer(rhs)) => self.t_eq(&self[*lhs], &self[*rhs]),
            (AstType::Function(lhs_args, lhs_ret), AstType::Function(rhs_args, rhs_ret)) => {
                self.t_eq(&self[*lhs_ret], &self[*rhs_ret])
                    && lhs_args
                        .zip(*rhs_args)
                        .all(|(lhs, rhs)| self.t_eq(&self[lhs], &self[rhs]))
            }
            (AstType::Struct(lhs), AstType::Struct(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    pub fn t_convet(&self, from: &AstType, to: &AstType) -> bool {
        if self.t_eq(from, to) {
            return true;
        }
        match (from, to) {
            (AstType::Never, _) => true,
            (_, AstType::Unit) => true,
            _ => false,
        }
    }
}

impl Index<ExprId> for Ast {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index.0]
    }
}

impl Index<AstFunctionId> for Ast {
    type Output = AstFunction;

    fn index(&self, index: AstFunctionId) -> &Self::Output {
        &self.functions[index.0]
    }
}

impl Index<AstTypeId> for Ast {
    type Output = AstType;

    fn index(&self, index: AstTypeId) -> &Self::Output {
        &self.types[index.0]
    }
}

impl IndexMut<AstTypeId> for Ast {
    fn index_mut(&mut self, index: AstTypeId) -> &mut Self::Output {
        &mut self.types[index.0]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstFunctionId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Args {
    pub names: Vec<String>,
    pub types: AstTypes,
}

impl Args {
    pub fn new(names: Vec<String>, types: AstTypes) -> Self {
        Self { names, types }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, AstTypeId)> {
        self.names.iter().zip(self.types)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstFunction {
    pub name: String,
    pub args: Args,
    pub ret_type: AstTypeId,
    pub body: Exprs,
    pub typ: AstType,
}

impl AstFunction {
    pub fn new(name: String, args: Args, ret_type: AstTypeId, body: Exprs) -> Self {
        Self {
            name,
            args: args.clone(),
            ret_type,
            body,
            typ: AstType::Function(args.types, ret_type),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub typ: AstTypeId,
}

impl Expr {
    pub fn new(kind: ExprKind, typ: AstTypeId) -> Self {
        Self { kind, typ }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Add(Exprs),
    Eq(ExprId, ExprId),
    Not(ExprId),
    Set(ExprId, ExprId),
    Let(String, AstTypeId, ExprId),
    Print(ExprId),
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Ident(String),
    Call(String, Exprs),
    Return(ExprId),
    If(ExprId, Exprs, Option<Exprs>),
    While(ExprId, Exprs),
    New(ExprId),
    Ref(ExprId),
    Deref(ExprId),
    FieldAccess(ExprId, String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ExprId(usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Exprs(ExprId, usize);

impl Exprs {
    pub fn range(&self) -> std::ops::Range<usize> {
        self.0 .0..(self.0 .0 + self.1)
    }
}

impl Iterator for Exprs {
    type Item = ExprId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.1 == 0 {
            None
        } else {
            self.1 -= 1;
            self.0 .0 += 1;
            Some(ExprId(self.0 .0 - 1))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstTypeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstTypes(pub AstTypeId, usize);

impl Iterator for AstTypes {
    type Item = AstTypeId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.1 == 0 {
            None
        } else {
            self.1 -= 1;
            self.0 .0 += 1;
            Some(AstTypeId(self.0 .0 - 1))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstType {
    Unit,
    Never,
    Int,
    Float,
    Bool,
    Char,
    Pointer(AstTypeId),
    Function(AstTypes, AstTypeId),
    Struct(String),
    Hole(usize),
}

impl AstType {
    pub fn to_string(&self) -> String {
        match self {
            AstType::Unit => "unit".to_string(),
            AstType::Never => "never".to_string(),
            AstType::Int => "int".to_string(),
            AstType::Float => "float".to_string(),
            AstType::Bool => "bool".to_string(),
            AstType::Function(_, _) => {
                unreachable!()
            }
            AstType::Hole(_) => "_".to_string(),
            AstType::Pointer(_) => unreachable!(),
            AstType::Struct(_) => todo!(),
            AstType::Char => "char".to_string(),
        }
    }

    pub fn to_ir(&self, builder: &Builder) -> IrType {
        match self {
            AstType::Unit => IrType::Unit,
            AstType::Never => IrType::Never,
            AstType::Int => IrType::Int,
            AstType::Float => IrType::Float,
            AstType::Bool => IrType::Bool,
            AstType::Char => IrType::Byte,
            AstType::Function(_, _) => todo!(),
            AstType::Hole(_) => unreachable!(),
            AstType::Pointer(_) => IrType::Pointer,
            AstType::Struct(s) => IrType::IrStruct(builder.get_struct(s).unwrap()),
        }
    }
}

impl Countable for AstType {
    fn from_count(count: usize) -> Self {
        Self::Hole(count)
    }
}

pub struct VariableId(pub usize);
pub struct FunctionId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub typ: AstTypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef {
    Struct(Struct),
}
