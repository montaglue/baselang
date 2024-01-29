use std::collections::HashMap;

use crate::ast::AstType;

pub struct TypeContext {
    pub variables: HashMap<String, AstType>,
    pub return_type: Option<AstType>,
    pub functions: HashMap<String, AstType>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            return_type: None,
            functions: HashMap::new(),
        }
    }

    pub fn add_variable(&mut self, name: String, ty: AstType) {
        self.variables.insert(name, ty);
    }

    pub fn add_function(&mut self, name: String, ty: AstType) {
        self.functions.insert(name, ty);
    }

    pub fn get_variable(&self, name: &str) -> Option<&AstType> {
        self.variables.get(name)
    }

    pub fn get_function(&self, name: &str) -> Option<&AstType> {
        self.functions.get(name)
    }
}
