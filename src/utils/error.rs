use std::{fmt::Display, rc::Rc};

use crate::{
    ast::{AstType, Expr},
    parser::tokenizer::{Token, TokenType},
};

use super::span::FSpan;
use inkwell::builder::BuilderError;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Default)]
pub struct CompilerError {
    pub message: Option<String>,
    pub span: Option<FSpan>,
    pub kind: ErrorKind,
}

impl CompilerError {
    pub fn new(kind: ErrorKind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }
    pub fn span(self, span: FSpan) -> Self {
        let span = Some(span);
        Self { span, ..self }
    }

    pub fn message(self, message: String) -> Self {
        let message = Some(message);
        Self { message, ..self }
    }

    pub fn from_llvm_builder(error: BuilderError) -> Self {
        Self {
            kind: ErrorKind::LLVMBuilderError(Rc::new(error)),
            ..Default::default()
        }
    }

    pub fn invariant_broken(message: String) -> Self {
        Self {
            kind: ErrorKind::InvariantBroken(message),
            ..Default::default()
        }
    }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}\n", self.kind)?;
        if let Some(message) = &self.message {
            writeln!(f, "Additional message: {}", message)?;
        }
        Ok(())
    }
}

pub type CompilerResult<T> = Result<T, CompilerError>;

#[derive(Error, Debug, Clone, PartialEq, Default)]
pub enum ErrorKind {
    #[error("Unexpected token: epxected {expected:?}, got {got:?}")]
    UnexpectedTokenType {
        expected: Vec<TokenType>,
        got: Token,
    },
    #[error("Unexpected type: {0}")]
    UnexpectedType(String),
    #[error("Unexpected end of source")]
    UnexpectedEndOfSource,
    #[error("sum of incompatible types")]
    SumIncompatibleTypes,
    #[error("No such variable: {0}")]
    NoSuchVariable(String),
    #[error("No such function: {0}")]
    NoSuchFunction(String),
    #[error("No such function or struct: {0}")]
    NoSuchFunctionOrStruct(String),
    #[error("Mismatched types: expected {expected:?}, got {got:?}")]
    MismatchedTypes { expected: AstType, got: AstType },
    #[error("Expected value, but got None")]
    ExpectedValue,
    #[error("Error during LLVM codegen {0}")]
    LLVMBuilderError(#[from] Rc<BuilderError>),
    #[error("Invariant broken: {0}")]
    InvariantBroken(String),
    #[error("Cannot make reference from {0:?}")]
    CantMakeReferenceFrom(Expr),
    #[error("Unexpected token: {0}")]
    UnexpectedToken(String),
    #[error("No such field: {0}")]
    NoSuchField(String),
    #[error("This is default error")]
    #[default]
    Default,
}

pub struct Errors {
    errors: Vec<CompilerError>,
    warnings: Vec<CompilerError>,
}

pub fn error<T>(kind: ErrorKind) -> CompilerResult<T> {
    Err(CompilerError::new(kind))
}

impl Errors {
    pub fn new() -> Self {
        Errors {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn error(&mut self, error: CompilerError) -> CompilerResult<()> {
        self.errors.push(error);
        Ok(())
    }

    pub fn warning(&mut self, warning: CompilerError) -> CompilerResult<()> {
        self.warnings.push(warning);
        Ok(())
    }
}
