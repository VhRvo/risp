use std::collections::HashMap;
use std::{fmt, io};

#[derive(Clone)]
enum RispExpr {
    Symbol(String),
    Number(f64),
    List(Vec<RispExpr>),
    Func(fn(&[RispExpr]) -> Result<RispExpr, RispErr>),
}

#[derive(Debug)]
enum RispErr {
    Reason(String),
}

#[derive(Clone)]
struct RispEnv {
    data: HashMap<String, RispExpr>,
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
        b")" => Err(RispErr::Reason("unexpected `)`".to_string())),
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
    let potential_float = token.parse::<f64>();
    match potential_float {
        Ok(v) => RispExpr::Number(v),
        Err(_) => RispExpr::Symbol(token.to_string()),
    }
}

impl RispEnv {
    fn default() -> Self {
        let mut data = HashMap::new();
        data.insert(
            "+".to_string(),
            RispExpr::Func(|args| {
                let sum = parse_list_of_floats(args)?
                    .iter()
                    .fold(0_f64, |sum, a| sum + a);
                Ok(RispExpr::Number(sum))
            }),
        );
        data.insert(
            "*".to_string(),
            RispExpr::Func(|args| {
                let sum = parse_list_of_floats(args)?
                    .iter()
                    .fold(1_f64, |sum, a| sum * a);
                Ok(RispExpr::Number(sum))
            }),
        );
        Self { data }
    }
}

impl Default for RispEnv {
    #[inline]
    fn default() -> Self {
        Self::default()
    }
}

fn parse_list_of_floats(args: &[RispExpr]) -> Result<Vec<f64>, RispErr> {
    args.iter().map(|expr| parse_single_float(expr)).collect()
}

fn parse_single_float(expr: &RispExpr) -> Result<f64, RispErr> {
    match expr {
        RispExpr::Number(number) => Ok(*number),
        _ => Err(RispErr::Reason("expected a number".to_string())),
    }
}

fn eval(expr: &RispExpr, env: &mut RispEnv) -> Result<RispExpr, RispErr> {
    match expr {
        RispExpr::Symbol(symbol) => env
            .data
            .get(symbol)
            .ok_or(RispErr::Reason(format!("unexpected symbol: '{}'", symbol)))
            .cloned(),
        RispExpr::Number(_) => Ok(expr.clone()),
        RispExpr::List(list) => {
            let (first, rest) = list
                .split_first()
                .ok_or(RispErr::Reason("empty list".to_string()))?;
            match eval(first, env)? {
                RispExpr::Func(func) => {
                    let evaluated_args =
                        rest.iter()
                            .map(|expr| eval(expr, env))
                            .collect::<Result<Vec<RispExpr>, RispErr>>()?;
                    func(&evaluated_args)
                }
                _ => Err(RispErr::Reason("expect function".to_string())),
            }
        }
        RispExpr::Func(_) => Err(RispErr::Reason("unexpected form".to_string())),
    }
}

impl fmt::Display for RispExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExpr::Symbol(symbol) => symbol.clone(),
            RispExpr::Number(number) => number.to_string(),
            RispExpr::List(list) => {
                let xs = list.iter().map(|expr| expr.to_string()).collect::<Vec<_>>();
                format!("{}", xs.join(", "))
            }
            RispExpr::Func(_) => "Function {}".to_string(),
        };
        write!(f, "{}", str)
    }
}

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExpr, RispErr> {
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
