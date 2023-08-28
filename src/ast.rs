use std::rc::Rc;
use std::fmt::Display;
use crate::builtins::{TRUE, NIL};
use crate::environment::Environment;

// --------------------------------------------------------------------------------------------

/// Errors that may appear during grammar code snippets invocation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QLispParseError {

    /// Appears when one cannot parse a given int in code into rust i64
    CannotParseInt(usize, usize),

    /// Appears when one cannot parse a given float in code into rust f64
    CannotParseFloat(usize, usize),
}

// --------------------------------------------------------------------------------------------

/// Exception structure to keep errors during interpretation (it is also s-exper)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exception {
    msg: String,
    frames: Vec<(usize, usize)>,
}

impl Exception {
    pub fn new(msg: String) -> Self {
        Self {
            msg, frames: vec![],
        }
    }
}

// --------------------------------------------------------------------------------------------

/// qulisp atomic objects
#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'code> {
    Int(i64),
    Float(f64),
    String(String),
    Symbol(&'code str),
    True,
    Nil,
}

impl<'code> From<i64> for Atom<'code> {
    fn from(value: i64) -> Self {
        Atom::Int(value)
    }
}

impl<'code> From<f64> for Atom<'code> {
    fn from(value: f64) -> Self {
        Atom::Float(value)
    }
}

impl<'code> From<String> for Atom<'code> {
    fn from(value: String) -> Self {
        Atom::String(value)
    }
}

impl<'code> From<&'code str> for Atom<'code> {
    fn from(value: &'code str) -> Self {
        Atom::Symbol(value)
    }
}

impl<'code> Display for Atom<'code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Int(val) => write!(f, "{}", val),
            Atom::Float(val) => write!(f, "{}", val),
            Atom::String(s) => write!(f, "\"{}\"", s),
            Atom::Symbol(name) => write!(f, "{}", *name),
            Atom::True => write!(f, "{TRUE}"),
            Atom::Nil => write!(f, "{NIL}"),
        }
    }
}

// --------------------------------------------------------------------------------------------

/// qulisp s-expression
#[derive(Debug, Clone)]
pub enum SExpr<'code> {
    List(Vec<Rc<SExpr<'code>>>, Option<(usize, usize)>),
    Pair(Rc<SExpr<'code>>, Rc<SExpr<'code>>, Option<(usize, usize)>),
    Atom(Atom<'code>, Option<(usize, usize)>),
    Environment(Rc<Environment<'code>>),
    Exception(Exception),
}

impl<'code> From<Atom<'code>> for SExpr<'code> {
    fn from(value: Atom<'code>) -> Self {
        SExpr::Atom(value, None)
    }
}

impl<'code> From<Vec<SExpr<'code>>> for SExpr<'code> {
    fn from(value: Vec<SExpr<'code>>) -> Self {
        let value = value.into_iter().map(|x| Rc::new(x)).collect();
        SExpr::List(value, None)
    }
}

impl<'code> From<&[SExpr<'code>]> for SExpr<'code> {
    fn from(value: &[SExpr<'code>]) -> Self {
        let value = value.into_iter().map(|x| Rc::new(x.clone())).collect();
        SExpr::List(value, None)
    }
}

impl<'code> From<(SExpr<'code>, SExpr<'code>)> for SExpr<'code> {
    fn from(value: (SExpr<'code>, SExpr<'code>)) -> Self {
        SExpr::Pair(Rc::new(value.0), Rc::new(value.1), None)
    }
}

impl<'code> From<i64> for SExpr<'code> {
    fn from(value: i64) -> Self {
        Atom::Int(value).into()
    }
}

impl<'code> From<f64> for SExpr<'code> {
    fn from(value: f64) -> Self {
        Atom::Float(value).into()
    }
}

impl<'code> From<String> for SExpr<'code> {
    fn from(value: String) -> Self {
        Atom::String(value).into()
    }
}

impl<'code> From<&'code str> for SExpr<'code> {
    fn from(value: &'code str) -> Self {
        Atom::Symbol(value).into()
    }
}

impl<'code> From<Exception> for SExpr<'code> {
    fn from(value: Exception) -> Self {
        SExpr::Exception(value)
    }
}

impl<'code> From<Rc<Environment<'code>>> for SExpr<'code> {
    fn from(value: Rc<Environment<'code>>) -> Self {
        SExpr::Environment(value)
    }
}

impl<'code> PartialEq for SExpr<'code> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (SExpr::List(lhs, ..), SExpr::List(rhs, ..)) => {
                lhs == rhs
            },
            (SExpr::Atom(lhs, ..), SExpr::Atom(rhs, ..)) => {
                lhs == rhs
            },
            (SExpr::Pair(lhs_a, lhs_b, ..), SExpr::Pair(rhs_a,rhs_b, ..)) => {
                (lhs_a == rhs_a) && (lhs_b == rhs_b)
            },
            (SExpr::Exception(lhs), SExpr::Exception(rhs)) => {
                lhs == rhs
            }
            // Do not take into account environments in order not to fall into infinite recursion
            (SExpr::Environment(_), SExpr::Environment(_)) => {
                true
            }
            _ => false,
        }
    }
}

impl<'code> Display for SExpr<'code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::Atom(atom, ..) => write!(f, "{}", atom),
            SExpr::List(list, ..) => {
                if list.is_empty() {
                    write!(f, "()")?;
                    return Ok(())
                }
                for (i, elem) in list.iter().enumerate() {
                    if i == 0
                    {
                        write!(f, "({} ", elem)?;
                    }
                    else if i == list.len() - 1
                    {
                        write!(f, "{})", elem)?;
                    }
                    else
                    {
                        write!(f, "{} ", elem)?;
                    }
                }
                Ok(())
            },
            SExpr::Pair(lhs, rhs, ..) => write!(f, "({} . {})", lhs, rhs),
            SExpr::Environment(env) => todo!(),
            SExpr::Exception(exc) => todo!(),
        }
    }
}

// --------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use lalrpop_util::ParseError;
    use crate::grammar::ProgramParser;
    use crate::qulisp_code_snippets::{
        SUM_OF_SQUARES,
        PRINT_MSG,
        TOO_BIG_INT,
        PRINT_QUOTED,
    };
    use super::SExpr;

    #[test]
    fn test_define_fn_ast()
    {
        let ast = ProgramParser::new().parse(SUM_OF_SQUARES).unwrap();
        let correct_ast: Vec<Rc<SExpr>> = vec![
            Rc::new(vec![
                "define".into(),
                vec![
                    "square".into(),
                    "x".into(),
                ].into(),
                vec![
                    "*".into(),
                    "x".into(),
                    "x".into(),
                ].into(),
            ].into()),
            Rc::new(vec![
                "define".into(),
                vec![
                    "sum-of-squares".into(),
                    "x".into(),
                    "y".into(),
                ].into(),
                vec![
                    "+".into(),
                    vec!["square".into(), "x".into()].into(),
                    vec!["square".into(), "y".into()].into(),
                ].into()
            ].into()),
            Rc::new(vec![
                "sum-of-squares".into(),
                0.00314f64.into(),
                40i64.into(),
            ].into()),
        ];
        assert_eq!(ast, correct_ast);
    }

    #[test]
    fn test_print_many_strings()
    {
        let ast = ProgramParser::new().parse(PRINT_MSG).unwrap();
        let correct_ast: Vec<Rc<SExpr>> = vec![
            Rc::new(vec![
                "define".into(),
                "print_msg".into(),
                vec![
                    "msg".into(),
                    "src".into(),
                    "dst".into(),
                ].into(),
                vec![
                    "print".into(),
                    "Message from ".to_string().into(),
                    "src".into(),
                    " to ".to_string().into(),
                    "dst".into(),
                    " : ".to_string().into(),
                    "msg".into(),
                ].into(),
            ].into()),
            Rc::new(vec![
                "print_msg".into(),
                "hello world".to_string().into(),
                "me".to_string().into(),
                "you".to_string().into(),
            ].into()),
        ];
        assert_eq!(ast, correct_ast);
    }

    #[test]
    fn test_to_big_number_ast()
    {
        let ast = ProgramParser::new().parse(TOO_BIG_INT);
        match ast.unwrap_err() {
            ParseError::User { error } => {
                match error {
                    super::QLispParseError::CannotParseInt(start, end) => {
                        assert_eq!(
                            "400000000000000000000000000000000000000000000000000000000000",
                            &TOO_BIG_INT[start..end],
                        );
                    },
                    other => panic!(
                        "Incorrect QLispParseError variant, must be CannotParseInt, got {:?}",
                        other
                    )
                }
            },
            other => panic!("Incorrect ParseError variant, must be User, got {:?}", other),
        }
    }

    #[test]
    fn test_print_quoted()
    {
        let ast = ProgramParser::new().parse(PRINT_QUOTED);
        assert_eq!("(print (quote (* 42 24)))", format!("{}", ast.unwrap()[0]));
    }
}