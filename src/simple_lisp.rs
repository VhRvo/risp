use crate::simple_lisp::data::RRResult;
use crate::simple_lisp::environment::RispEnv;
use crate::simple_lisp::error::RispErr::Reason;
use crate::simple_lisp::interpreter::eval;
use crate::simple_lisp::parser::{parse, tokenize};

use std::io;

pub mod data;
pub mod environment;
pub mod error;
pub mod interpreter;
pub mod parser;
pub mod printer;

fn parse_eval(expr: String, env: &mut RispEnv) -> RRResult {
    let (parsed_expr, _) = parse(&tokenize(expr))?;
    eval(&parsed_expr, env)
}

fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");
    expr
}

pub(crate) fn main() {
    let env = &mut RispEnv::default();
    loop {
        println!("risp> ");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(result) => {
                println!("// 🔥 => {}", result);
            }
            Err(Reason(message)) => println!("// 😱 => {}", message),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::simple_lisp::data::RispExpr;

    #[test]
    fn dynamic_scope() {
        let mut env = Default::default();
        let def1 = "(define add-y (lambda (x) (+ 1 y)))";
        let def2 = "(define y 10)";
        let exp2 = "(add-y 2)";
        assert!(parse_eval(def1.to_string(), &mut env).is_ok());
        assert!(parse_eval(def2.to_string(), &mut env).is_ok());
        assert_eq!(
            parse_eval(exp2.to_string(), &mut env),
            Ok(RispExpr::Number(11_f64))
        );
    }
}
