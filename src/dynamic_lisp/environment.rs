use crate::dynamic_lisp::data::RRResult;
use crate::dynamic_lisp::data::RispExpr;
use crate::dynamic_lisp::error::RispErr;
use crate::dynamic_lisp::parser::parse_list_of_floats;

use std::collections::HashMap;

#[derive(Clone)]
pub struct RispEnv<'a> {
    pub(crate) env: HashMap<String, RispExpr>,
    pub(crate) outer: Option<&'a RispEnv<'a>>,
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
    pub(crate) fn get(&self, key: &str) -> Option<RispExpr> {
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
