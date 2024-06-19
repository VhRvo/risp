use crate::dynamic_lisp::data::{RRResult, RResult, RispExpr, RispLambda};
use crate::dynamic_lisp::environment::RispEnv;
use crate::dynamic_lisp::error::RispErr::Reason;
use crate::dynamic_lisp::error::{error, RispErr};
use crate::dynamic_lisp::parser::parse_list_of_symbol_strings;

use std::collections::HashMap;
use std::rc::Rc;

pub(crate) fn eval(expr: &RispExpr, env: &mut RispEnv) -> RRResult {
    match expr {
        RispExpr::Boolean(_) => Ok(expr.clone()),
        RispExpr::Symbol(symbol) => env
            .get(symbol)
            .ok_or(Reason(format!("unexpected symbol: '{}'", symbol))),
        RispExpr::Number(_) => Ok(expr.clone()),
        RispExpr::List(list) => {
            let (first, rest) = list.split_first().ok_or(Reason(
                "function application expected a non-empty list".to_string(),
            ))?;
            match eval_special_form(first, rest, env) {
                Some(result) => result,
                None => match eval(first, env)? {
                    RispExpr::Func(func) => {
                        let evaluated_args = eval_list(rest, env)?;
                        func(&evaluated_args)
                    }
                    RispExpr::Lambda(lambda) => {
                        let new_env = &mut env_for_lambda(lambda.params, rest, env)?;
                        eval(&lambda.body, new_env)
                    }
                    _ => error("expect function in application"),
                },
            }
        }
        RispExpr::Func(_) => error("unexpected form"),
        RispExpr::Lambda(_) => error("unexpected form"),
    }
}

fn eval_list(args: &[RispExpr], env: &mut RispEnv) -> RResult<Vec<RispExpr>> {
    args.iter()
        .map(|expr| eval(expr, env))
        .collect::<Result<Vec<RispExpr>, RispErr>>()
}

fn env_for_lambda<'a>(
    params: Rc<RispExpr>,
    args: &[RispExpr],
    outer: &'a mut RispEnv,
) -> RResult<RispEnv<'a>> {
    let names = parse_list_of_symbol_strings(params)?;
    if names.len() != args.len() {
        Err(Reason(format!(
            "expected {} arguments, got {}",
            names.len(),
            args.len()
        )))
    } else {
        let values = eval_list(args, outer)?;
        let mut env = HashMap::new();
        for (key, value) in names.into_iter().zip(values.into_iter()) {
            env.insert(key, value);
        }
        Ok(RispEnv {
            env,
            outer: Some(outer),
        })
    }
}

fn eval_special_form(
    expr: &RispExpr,
    args: &[RispExpr],
    env: &mut RispEnv,
) -> Option<Result<RispExpr, RispErr>> {
    match expr {
        RispExpr::Symbol(symbol) => match symbol.as_bytes() {
            b"if" => Some(eval_if(args, env)),
            b"lambda" => Some(eval_lambda(args)),
            b"define" => Some(eval_define(args, env)),
            _ => None,
        },
        _ => None,
    }
}

fn eval_if(args: &[RispExpr], env: &mut RispEnv) -> RRResult {
    match args {
        [test, true_branch, false_branch] => match eval(test, env)? {
            RispExpr::Boolean(b) => {
                if b {
                    eval(true_branch, env)
                } else {
                    eval(false_branch, env)
                }
            }
            _ => error("expected boolean in `if`'s condition"),
        },
        _ => error("unexpected number of `if` form"),
    }
}

fn eval_define(args: &[RispExpr], env: &mut RispEnv) -> RRResult {
    match args {
        [name, expr] => match name {
            RispExpr::Symbol(symbol) => {
                let result = eval(expr, env)?;
                env.env.insert(symbol.clone(), result);
                Ok(RispExpr::Symbol(symbol.clone()))
            }
            _ => error("expected identifier of `define` form"),
        },
        _ => error("unexpected number of `define` form"),
    }
}

fn eval_lambda(args: &[RispExpr]) -> RRResult {
    match args {
        [params, body] => Ok(RispExpr::Lambda(RispLambda {
            params: Rc::new(params.clone()),
            body: Rc::new(body.clone()),
        })),
        _ => error("unexpected number of `lambda` form"),
    }
}
