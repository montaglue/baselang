use std::{
    collections::HashMap,
    iter::{Map, Peekable},
    str::SplitWhitespace,
};

use crate::{
    ast::{Args, Ast, AstFunction, AstType, Enum, Expr, ExprKind, Field, Struct},
    parser::tokenizer::TokenType,
    utils::error::{error, CompilerResult, ErrorKind, Errors},
};

use self::tokenizer::{Token, Tokenizer};

pub mod tokenizer;

pub struct Parser<'s, T: Iterator<Item = (usize, &'s str)>> {
    source: &'s str,
    tokenizer: Peekable<Tokenizer<'s, T>>,
}

impl<'s> Parser<'s, Map<SplitWhitespace<'_>, fn(&str) -> (usize, &'s str)>> {
    pub fn new(source: &'s str) -> Parser<impl Iterator<Item = (usize, &'s str)>> {
        Parser {
            source,
            tokenizer: Tokenizer::new(source).peekable(),
        }
    }
}

pub fn expect_token(
    token: Option<Token>,
    pattern: Option<TokenType>,
    _errors: &mut Errors,
) -> CompilerResult<Token> {
    let Some(token) = token else {
        return error(ErrorKind::UnexpectedEndOfSource);
    };
    if let Some(pattern) = pattern {
        if token.kind != pattern {
            return error(ErrorKind::UnexpectedTokenType {
                expected: vec![pattern],
                got: token,
            });
        }
    }
    Ok(token)
}

impl<'s, T> Parser<'s, T>
where
    T: Iterator<Item = (usize, &'s str)>,
{
    fn parse_expr(&mut self, ast: &mut Ast, errors: &mut Errors) -> CompilerResult<Expr> {
        let token = self.tokenizer.next().unwrap();
        Ok(match token.kind {
            TokenType::Integer(i) => Expr::new(ExprKind::Integer(i), ast.add_type(AstType::Int)),
            TokenType::Float(f) => Expr::new(ExprKind::Float(f), ast.add_type(AstType::Float)),
            TokenType::Ident => {
                let ident = self.source[token.span.range()].to_owned();
                if ident.starts_with('"') && ident.ends_with('"') {
                    return Ok(Expr::new(
                        ExprKind::String(ident),
                        ast.add_type(AstType::Struct("String".to_owned())),
                    ));
                }
                Expr::new(ExprKind::Ident(ident), ast.new_type())
            }
            TokenType::OpenParen => {
                let token = expect_token(self.tokenizer.next(), None, errors)?;

                let name = self.source[token.span.range()].to_owned();
                if "let" == &name {
                    let name = expect_token(self.tokenizer.next(), Some(TokenType::Ident), errors)?;
                    let typ = self.parse_type(ast, errors)?;
                    let typ = ast.add_type(typ);
                    let value = self.parse_expr(ast, errors)?;
                    expect_token(self.tokenizer.next(), Some(TokenType::CloseParen), errors)?;
                    return Ok(Expr::new(
                        ExprKind::Let(
                            self.source[name.span.range()].to_owned(),
                            typ,
                            ast.add_expr(value),
                        ),
                        ast.add_type(AstType::Unit),
                    ));
                }

                let mut exprs: Vec<Vec<_>> = vec![Vec::new()];
                while self
                    .tokenizer
                    .peek()
                    .is_some_and(|token| token.kind != TokenType::CloseParen)
                {
                    if self.tokenizer.peek().unwrap().kind == TokenType::Border {
                        exprs.push(Vec::new());
                        self.tokenizer.next();
                        continue;
                    }
                    let expr = self.parse_expr(ast, errors)?;
                    exprs.last_mut().unwrap().push(expr);
                }
                expect_token(self.tokenizer.next(), Some(TokenType::CloseParen), errors)?; // skip )

                match name.as_str() {
                    "eq" => {
                        let rhs = ast.add_expr(exprs[0].pop().unwrap());
                        let lhs = ast.add_expr(exprs[0].pop().unwrap());
                        Expr::new(ExprKind::Eq(lhs, rhs), ast.add_type(AstType::Bool))
                    }
                    "plus" => Expr::new(
                        ExprKind::Add(ast.add_exprs(exprs.pop().unwrap())),
                        ast.new_type(),
                    ),
                    "set" => {
                        let Some(rhs) = exprs[0].pop() else {
                            unreachable!()
                        };
                        let Some(lhs) = exprs[0].pop() else {
                            unreachable!()
                        };
                        Expr::new(
                            ExprKind::Set(ast.add_expr(lhs), ast.add_expr(rhs)),
                            ast.add_type(AstType::Unit),
                        )
                    }
                    "print" => {
                        let Some(value) = exprs[0].pop() else {
                            unreachable!()
                        };
                        Expr::new(
                            ExprKind::Print(ast.add_expr(value)),
                            ast.add_type(AstType::Unit),
                        )
                    }
                    "return" => {
                        let Some(value) = exprs[0].pop() else {
                            unreachable!()
                        };
                        Expr::new(
                            ExprKind::Return(ast.add_expr(value)),
                            ast.add_type(AstType::Never),
                        )
                    }
                    "if" => {
                        let (cond, then, els) = if exprs.len() == 3 {
                            let Some(els) = exprs.pop() else {
                                unreachable!()
                            };
                            let Some(then) = exprs.pop() else {
                                unreachable!()
                            };
                            let Some(cond) = exprs.pop().unwrap().pop() else {
                                unreachable!()
                            };
                            (cond, then, Some(els))
                        } else {
                            let Some(then) = exprs.pop() else {
                                unreachable!()
                            };
                            let Some(cond) = exprs.pop().unwrap().pop() else {
                                unreachable!()
                            };
                            let els = None;
                            (cond, then, els)
                        };

                        let cond = ast.add_expr(cond);
                        let then = ast.add_exprs(then);
                        let els = els.map(|els| ast.add_exprs(els));
                        Expr::new(ExprKind::If(cond, then, els), ast.add_type(AstType::Unit))
                    }
                    "while" => {
                        let Some(body) = exprs.pop() else {
                            unreachable!()
                        };
                        let Some(cond) = exprs.pop().unwrap().pop() else {
                            unreachable!()
                        };

                        let cond = ast.add_expr(cond);
                        let body = ast.add_exprs(body);
                        Expr::new(ExprKind::While(cond, body), ast.add_type(AstType::Unit))
                    }
                    "not" => {
                        let Some(value) = exprs[0].pop() else {
                            unreachable!()
                        };
                        Expr::new(
                            ExprKind::Not(ast.add_expr(value)),
                            ast.add_type(AstType::Bool),
                        )
                    }
                    "new" => {
                        let Some(value) = exprs[0].pop() else {
                            unreachable!()
                        };
                        let typ = ast.add_type(AstType::Pointer(value.typ));

                        let expr = ast.add_expr(value);
                        Expr::new(ExprKind::New(expr), typ)
                    }
                    "ref" => {
                        let Some(value) = exprs[0].pop() else {
                            unreachable!()
                        };
                        let typ = ast.add_type(AstType::Pointer(value.typ));

                        let expr = ast.add_expr(value);
                        Expr::new(ExprKind::Ref(expr), typ)
                    }
                    "deref" => {
                        let Some(value) = exprs[0].pop() else {
                            unreachable!()
                        };
                        Expr::new(ExprKind::Deref(ast.add_expr(value)), ast.new_type())
                    }
                    "field" => {
                        let Some(Expr {
                            kind: ExprKind::Ident(field),
                            typ: _,
                        }) = exprs[0].pop()
                        else {
                            unreachable!()
                        };
                        let Some(expr) = exprs[0].pop() else {
                            unreachable!()
                        };

                        let expr = ast.add_expr(expr);
                        Expr::new(ExprKind::FieldAccess(expr, field), ast.new_type())
                    }
                    _ => Expr::new(
                        ExprKind::Call(name, ast.add_exprs(exprs.pop().unwrap())),
                        ast.new_type(),
                    ),
                }
            }
            TokenType::CloseParen => unreachable!(),
            TokenType::Border => unreachable!(),
        })
    }

    fn parse_type(&mut self, ast: &mut Ast, errors: &mut Errors) -> CompilerResult<AstType> {
        let token = self.tokenizer.next();
        let token = expect_token(token, None, errors)?;

        match token.kind {
            TokenType::Ident => {
                let type_name = &self.source[token.span.range()];

                match type_name {
                    "bool" => Ok(AstType::Bool),
                    "float" => Ok(AstType::Float),
                    "int" => Ok(AstType::Int),
                    "unit" => Ok(AstType::Unit),
                    "char" => Ok(AstType::Char),
                    "_" => {
                        let hole_id = ast.new_type();
                        Ok(ast[hole_id].clone())
                    }
                    s => Ok(AstType::Struct(s.to_owned())), // TOOD: make name validation
                }
            }
            TokenType::OpenParen => {
                // skip ptr
                expect_token(self.tokenizer.next(), Some(TokenType::Ident), errors)?;
                let typ = self.parse_type(ast, errors)?;
                ast.add_type(typ.clone());
                expect_token(self.tokenizer.next(), Some(TokenType::CloseParen), errors)?;
                Ok(AstType::Pointer(ast.add_type(typ)))
            }
            _ => error(ErrorKind::UnexpectedTokenType {
                expected: vec![TokenType::Ident, TokenType::OpenParen],
                got: token,
            }),
        }
    }

    fn parse_args(&mut self, ast: &mut Ast, errors: &mut Errors) -> CompilerResult<Args> {
        expect_token(self.tokenizer.next(), Some(TokenType::OpenParen), errors)?; // skip (
        let mut names = Vec::new();
        let mut args_types = Vec::new();
        while let Some(token) = self.tokenizer.next() {
            match token.kind {
                TokenType::Ident => {
                    let ident = self.source[token.span.range()].to_owned();
                    names.push(ident);

                    let typ = self.parse_type(ast, errors)?;
                    args_types.push(typ);
                }
                TokenType::CloseParen => break,
                _ => {
                    return error(ErrorKind::UnexpectedTokenType {
                        expected: vec![TokenType::Ident, TokenType::CloseParen],
                        got: token,
                    });
                }
            }
        }
        Ok(Args {
            names,
            types: ast.add_types(args_types),
        })
    }

    fn parse_fn(&mut self, ast: &mut Ast, errors: &mut Errors) -> CompilerResult<AstFunction> {
        let name_token = expect_token(self.tokenizer.next(), Some(TokenType::Ident), errors)?;
        let name = self.source[name_token.span.range()].to_owned();
        // TODO: check is name valid

        let args = self.parse_args(ast, errors)?;

        let ret_type = self.parse_type(ast, errors)?;
        let ret_type = ast.add_type(ret_type);

        let mut exprs = Vec::new();

        while self
            .tokenizer
            .peek()
            .is_some_and(|token| token.kind != TokenType::CloseParen)
        {
            let expr = self.parse_expr(ast, errors)?;
            exprs.push(expr);
        }

        expect_token(self.tokenizer.next(), Some(TokenType::CloseParen), errors)?; // skip )
        let body = ast.add_exprs(exprs);

        Ok(AstFunction::new(name, args, ret_type, body))
    }

    fn parse_struct(&mut self, ast: &mut Ast, errors: &mut Errors) -> CompilerResult<Struct> {
        let name_token = expect_token(self.tokenizer.next(), Some(TokenType::Ident), errors)?;
        let name = self.source[name_token.span.range()].to_owned();

        let mut fields = Vec::new();

        while self
            .tokenizer
            .peek()
            .is_some_and(|token| token.kind != TokenType::CloseParen)
        {
            let field_name_token =
                expect_token(self.tokenizer.next(), Some(TokenType::Ident), errors)?;

            let field_name = self.source[field_name_token.span.range()].to_owned();

            let field_type = self.parse_type(ast, errors)?;
            let field_type = ast.add_type(field_type);

            let field = Field {
                name: field_name,
                typ: field_type,
            };

            fields.push(field);
        }

        expect_token(self.tokenizer.next(), Some(TokenType::CloseParen), errors)?; // skip )

        Ok(Struct { name, fields })
    }

    fn parse_enum(&mut self, ast: &mut Ast, errors: &mut Errors) -> CompilerResult<Enum> {
        let name_token = expect_token(self.tokenizer.next(), Some(TokenType::Ident), errors)?;
        let name = self.source[name_token.span.range()].to_owned();

        let mut variants = HashMap::new();

        while self
            .tokenizer
            .peek()
            .is_some_and(|token| token.kind != TokenType::CloseParen)
        {
            let variant_name_token =
                expect_token(self.tokenizer.next(), Some(TokenType::Ident), errors)?;

            let variant_name = self.source[variant_name_token.span.range()].to_owned();

            let variant_type = self.parse_type(ast, errors)?;
            let variant_type = ast.add_type(variant_type);

            variants.insert(variant_name, variant_type);
        }

        expect_token(self.tokenizer.next(), Some(TokenType::CloseParen), errors)?; // skip )

        let enm = Enum { name, variants };

        Ok(enm)
    }
}

impl<'s, T> Parser<'s, T>
where
    T: Iterator<Item = (usize, &'s str)>,
{
    pub fn parse(&mut self, errors: &mut Errors) -> CompilerResult<Ast> {
        let mut ast = Ast::new();
        while self.tokenizer.peek().is_some() {
            expect_token(self.tokenizer.next(), Some(TokenType::OpenParen), errors)?;
            let token = expect_token(self.tokenizer.next(), Some(TokenType::Ident), errors)?;

            let token = &self.source[token.span.range()];

            match token {
                "fn" => {
                    let func = self.parse_fn(&mut ast, errors)?;
                    ast.add_function(func);
                }
                "struct" => {
                    let strct = self.parse_struct(&mut ast, errors)?;
                    ast.add_struct(strct);
                }
                "enum" => {
                    let enm = self.parse_enum(&mut ast, errors)?;
                    ast.add_enum(enm);
                }
                _ => {
                    return error(ErrorKind::UnexpectedToken(token.to_owned()));
                }
            }
        }
        Ok(ast)
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::tokenizer, utils::error::Errors};

    #[test]
    fn one_plus_one() {
        let parser = super::Parser::new(include_str!("../../examples/one_plus_one.base"));
        println!("{:?}", parser.source.len());
        for token in parser.tokenizer {
            println!("{:?}", token);
        }
        let mut parser = super::Parser::new(include_str!("../../examples/one_plus_one.base"));
        let mut errors = Errors::new();
        let ast = parser.parse(&mut errors).unwrap();
        println!("{:#?}", ast);
    }

    #[test]
    fn multiple_roots() {
        let mut parser = super::Parser::new(include_str!("../../examples/multiple_roots.base"));
        let mut errors = Errors::new();
        let ast = parser.parse(&mut errors).unwrap();
        println!("{:#?}", ast);
    }

    #[test]
    fn set() {
        let mut parser = super::Parser::new(include_str!("../../examples/set.base"));
        let mut errors = Errors::new();
        let ast = parser.parse(&mut errors).unwrap();
        println!("{:#?}", ast);
    }

    #[test]
    fn ident() {
        let mut parser = super::Parser::new(include_str!("../../examples/ident.base"));
        let mut errors = Errors::new();
        let ast = parser.parse(&mut errors).unwrap();
        println!("{:#?}", ast);
    }

    #[test]
    fn set_ident() {
        let mut parser = super::Parser::new(include_str!("../../examples/set_ident.base"));
        let mut errors = Errors::new();

        let ast = parser.parse(&mut errors).unwrap();
        println!("{:#?}", ast);
    }

    #[test]
    fn functions() {
        let src = include_str!("../../examples/functions.base");
        let tokenizer = tokenizer::Tokenizer::new(include_str!("../../examples/functions.base"));
        for token in tokenizer {
            println!("{:?}", &src[token.span.range()]);
            println!("{:?}", token);
        }
        let mut parser = super::Parser::new(include_str!("../../examples/functions.base"));
        let mut errors = Errors::new();

        let ast = parser.parse(&mut errors).unwrap();
        println!("{:#?}", ast);
    }

    #[test]
    fn if_expr() {
        let src = include_str!("../../examples/if.base");
        let tokenizer = tokenizer::Tokenizer::new(include_str!("../../examples/if.base"));
        for token in tokenizer {
            println!("{:?}", &src[token.span.range()]);
            println!("{:?}", token);
        }
        let mut parser = super::Parser::new(include_str!("../../examples/if.base"));
        let mut errors = Errors::new();

        let ast = parser.parse(&mut errors).unwrap();
        println!("{:#?}", ast);
    }

    #[test]
    fn recursion() {
        let mut parser = super::Parser::new(include_str!("../../examples/recursion.base"));
        let mut errors = Errors::new();
        let ast = parser.parse(&mut errors).unwrap();
        println!("{:#?}", ast);
    }
}
