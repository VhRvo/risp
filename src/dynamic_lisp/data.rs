use crate::dynamic_lisp::error::RispErr;

use std::rc::Rc;

pub type RResult<Value> = Result<Value, RispErr>;
pub type RRResult = RResult<RispExpr>;

#[derive(Clone, PartialEq, Debug)]
pub enum RispExpr {
    Boolean(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExpr>),
    Func(fn(&[RispExpr]) -> RRResult),
    Lambda(RispLambda),
}

#[derive(Clone, PartialEq, Debug)]
pub struct RispLambda {
    pub(crate) params: Rc<RispExpr>,
    pub(crate) body: Rc<RispExpr>,
}
