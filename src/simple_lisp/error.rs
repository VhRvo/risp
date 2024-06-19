use crate::simple_lisp::data::RResult;

#[derive(Debug, PartialEq)]
pub enum RispErr {
    Reason(String),
}

#[inline]
pub(crate) fn error<Value>(str: &str) -> RResult<Value> {
    Err(RispErr::Reason(str.to_string()))
}
