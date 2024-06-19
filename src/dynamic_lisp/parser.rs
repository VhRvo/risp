use crate::dynamic_lisp::data::{RResult, RispExpr};
use crate::dynamic_lisp::error;
use crate::dynamic_lisp::error::RispErr::Reason;
use crate::dynamic_lisp::error::{error, RispErr};

use std::rc::Rc;

pub(crate) fn tokenize(expr: String) -> Vec<String> {
    let result = expr
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|str| str.to_string())
        .collect();
    // println!("{:?}", result);
    result
}

pub(crate) fn parse(tokens: &[String]) -> Result<(RispExpr, &[String]), RispErr> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(Reason("could not get token".to_string()))?;
    match token.as_bytes() {
        b"(" => read_seq(rest),
        b")" => error::error("unexpected `)`"),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq(tokens: &[String]) -> Result<(RispExpr, &[String]), RispErr> {
    let mut result: Vec<RispExpr> = vec![];
    let mut tokens = tokens;
    loop {
        let (next_token, rest) = tokens
            .split_first()
            .ok_or(Reason("could not find closing `)`".to_string()))?;
        if next_token == ")" {
            return Ok((RispExpr::List(result), rest)); // skip `)`, head to the token after
        }
        let (expr, rest_tokens) = parse(tokens)?;
        result.push(expr);
        tokens = rest_tokens;
    }
}

fn parse_atom(token: &str) -> RispExpr {
    match token {
        "true" => RispExpr::Boolean(true),
        "false" => RispExpr::Boolean(false),
        _ => {
            let potential_float = token.parse::<f64>();
            match potential_float {
                Ok(v) => RispExpr::Number(v),
                Err(_) => RispExpr::Symbol(token.to_string()),
            }
        }
    }
}

pub(crate) fn parse_list_of_floats(args: &[RispExpr]) -> Result<Vec<f64>, RispErr> {
    args.iter().map(|expr| parse_single_float(expr)).collect()
}

fn parse_single_float(expr: &RispExpr) -> Result<f64, RispErr> {
    match expr {
        RispExpr::Number(number) => Ok(*number),
        _ => error::error("expected a number"),
    }
}

pub(crate) fn parse_list_of_symbol_strings(params: Rc<RispExpr>) -> RResult<Vec<String>> {
    let list = match params.as_ref() {
        RispExpr::List(list) => Ok(list.clone()),
        _ => error("expected args form to be a list"),
    }?;
    list.iter()
        .map(|item| match item {
            RispExpr::Symbol(symbol) => Ok(symbol.clone()),
            _ => error("expected symbols in the argument list"),
        })
        .collect()
}
