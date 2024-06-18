use crate::risp_calculator::RispErr::Reason;
use std::collections::HashMap;
use std::rc::Rc;
use std::{fmt, io};

type RResult<Value> = Result<Value, RispErr>;
type RRResult = RResult<RispExpr>;

#[derive(Clone)]
enum RispExpr {
    Boolean(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExpr>),
    Func(fn(&[RispExpr]) -> RRResult),
    Lambda(RispLambda),
}

#[derive(Clone)]
struct RispLambda {
    params: Rc<RispExpr>,
    body: Rc<RispExpr>,
}

#[derive(Debug)]
enum RispErr {
    Reason(String),
}

#[derive(Clone)]
struct RispEnv<'a> {
    env: HashMap<String, RispExpr>,
    outer: Option<&'a RispEnv<'a>>,
}

fn tokenize(expr: String) -> Vec<String> {
    let result = expr
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|str| str.to_string())
        .collect();
    // println!("{:?}", result);
    result
}

fn parse(tokens: &[String]) -> Result<(RispExpr, &[String]), RispErr> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(RispErr::Reason("could not get token".to_string()))?;
    match token.as_bytes() {
        b"(" => read_seq(rest),
        b")" => error("unexpected `)`"),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq(tokens: &[String]) -> Result<(RispExpr, &[String]), RispErr> {
    let mut result: Vec<RispExpr> = vec![];
    let mut tokens = tokens;
    loop {
        let (next_token, rest) = tokens
            .split_first()
            .ok_or(RispErr::Reason("could not find closing `)`".to_string()))?;
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

macro_rules! ensure_monotonic {
    ($check_fn:expr) => {{
        |args: &[RispExpr]| -> RRResult {
            let floats = parse_list_of_floats(args)?;
            let (first, rest) = floats
                .split_first()
                .ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let result = rest
                .iter()
                .fold((true, first), |(result, prev), item| {
                    ((result && $check_fn(prev, item)), item)
                })
                .0;
            Ok(RispExpr::Boolean(result))
        }
    }};
}

impl RispEnv<'_> {
    fn get(&self, key: &str) -> Option<RispExpr> {
        match self.env.get(key) {
            Some(value) => Some(value.clone()),
            None => self.outer.and_then(|outer| outer.get(key)),
        }
    }
}

impl Default for RispEnv<'_> {
    #[inline]

    fn default() -> Self {
        let mut env = HashMap::new();
        env.insert(
            "+".to_string(),
            RispExpr::Func(|args| {
                let sum = parse_list_of_floats(args)?
                    .iter()
                    .fold(0_f64, |sum, a| sum + a);
                Ok(RispExpr::Number(sum))
            }),
        );
        env.insert(
            "*".to_string(),
            RispExpr::Func(|args| {
                let sum = parse_list_of_floats(args)?
                    .iter()
                    .fold(1_f64, |sum, a| sum * a);
                Ok(RispExpr::Number(sum))
            }),
        );
        env.insert(
            "=".to_string(),
            RispExpr::Func(ensure_monotonic!(|left, right| left == right)),
        );
        env.insert(
            "<".to_string(),
            RispExpr::Func(ensure_monotonic!(|left, right| left < right)),
        );
        env.insert(
            ">".to_string(),
            RispExpr::Func(ensure_monotonic!(|left, right| left > right)),
        );
        env.insert(
            "<=".to_string(),
            RispExpr::Func(ensure_monotonic!(|left, right| left <= right)),
        );
        env.insert(
            ">=".to_string(),
            RispExpr::Func(ensure_monotonic!(|left, right| left >= right)),
        );
        Self { env, outer: None }
    }
}

fn parse_list_of_floats(args: &[RispExpr]) -> Result<Vec<f64>, RispErr> {
    args.iter().map(|expr| parse_single_float(expr)).collect()
}

fn parse_single_float(expr: &RispExpr) -> Result<f64, RispErr> {
    match expr {
        RispExpr::Number(number) => Ok(*number),
        _ => error("expected a number"),
    }
}

#[inline]
fn error<Value>(str: &str) -> RResult<Value> {
    Err(RispErr::Reason(str.to_string()))
}

fn eval(expr: &RispExpr, env: &mut RispEnv) -> RRResult {
    match expr {
        RispExpr::Boolean(_) => Ok(expr.clone()),
        RispExpr::Symbol(symbol) => env
            .get(symbol)
            .ok_or(RispErr::Reason(format!("unexpected symbol: '{}'", symbol))),
        RispExpr::Number(_) => Ok(expr.clone()),
        RispExpr::List(list) => {
            let (first, rest) = list.split_first().ok_or(RispErr::Reason(
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

fn parse_list_of_symbol_strings(params: Rc<RispExpr>) -> RResult<Vec<String>> {
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

impl fmt::Display for RispExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExpr::Boolean(b) => b.to_string(),
            RispExpr::Symbol(symbol) => symbol.clone(),
            RispExpr::Number(number) => number.to_string(),
            RispExpr::List(list) => {
                let xs = list.iter().map(|expr| expr.to_string()).collect::<Vec<_>>();
                format!("{}", xs.join(", "))
            }
            RispExpr::Func(_) => "Function {}".to_string(),
            RispExpr::Lambda(_) => "Lambda {}".to_string(),
        };
        write!(f, "{}", str)
    }
}

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
            Err(RispErr::Reason(message)) => println!("// 😱 => {}", message),
        }
    }
}
