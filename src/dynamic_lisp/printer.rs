use crate::dynamic_lisp::data::RispExpr;

use std::fmt;

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
