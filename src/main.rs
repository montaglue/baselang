use std::process::Command;

use backend::build;
use utils::error::CompilerResult;

use crate::{ir::irgen::generate_ir, parser::Parser, utils::error::Errors};

pub mod ast;
pub mod backend;
pub mod ir;
pub mod parser;
pub mod resolve;
pub mod utils;

fn process_error<T>(error: impl Into<CompilerResult<T>>, prompt: &str) -> T {
    let error = error.into();
    match error {
        Ok(value) => value,
        Err(err) => {
            eprintln!("{}: {}", prompt, err);
            std::process::exit(0);
        }
    }
}

fn main() {
    let program = include_str!("../examples/enum.base");

    let mut errors = Errors::new();

    let mut parser = Parser::new(program);

    let mut ast = process_error(parser.parse(&mut errors), "Error during parsing");
    println!("{:#?}", ast);

    process_error(
        resolve::resolve(&mut ast, &mut errors),
        "Error during resolving",
    );

    let ir = process_error(generate_ir(&ast, &mut errors), "Error during IR generation");
    // println!("{:#?}", ir);

    let context = inkwell::context::Context::create();

    let module = process_error(build(&ir, &context, &mut errors), "Error during codegen");

    if let Err(err) = module.print_to_file("main.ll") {
        eprintln!("Error during writing to file: {}", err);
        return;
    }

    let out = match Command::new("lli-16").arg("main.ll").output() {
        Ok(out) => out,
        Err(err) => {
            eprintln!("Error running command: {}", err);
            return;
        }
    };

    if !out.status.success() {
        eprintln!(
            "Error running interpreter: {}",
            String::from_utf8(out.stderr).unwrap()
        );
        return;
    }
    print!("{}", String::from_utf8(out.stdout).unwrap());
}
