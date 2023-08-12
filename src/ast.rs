use std::rc::Rc;
use std::fmt::Display;
use std::ops::{Deref, DerefMut};

// --------------------------------------------------------------------------------------------

/// Errors that may appear during grammar code snippets invocation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QLispParseError {

    /// Appears when one cannot parse a given int in code into rust i64
    CannotParseInt(usize, usize),

    /// Appears when one cannot parse a given float in code into rust f64
    CannotParseFloat(usize, usize),
}

// --------------------------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Symbol(pub String);

impl Deref for Symbol {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Symbol {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<S> From<S> for Symbol
where
    S: AsRef<str>
{
    fn from(value: S) -> Self {
        Symbol(value.as_ref().to_owned())
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// --------------------------------------------------------------------------------------------

/// qulisp atomic objects
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Int(i64),
    Float(f64),
    String(String),
    Symbol(Symbol),
}

impl From<Symbol> for Atom {
    fn from(value: Symbol) -> Self {
        Atom::Symbol(value)
    }
}

impl From<i64> for Atom {
    fn from(value: i64) -> Self {
        Atom::Int(value)
    }
}

impl From<f64> for Atom {
    fn from(value: f64) -> Self {
        Atom::Float(value)
    }
}

impl From<String> for Atom {
    fn from(value: String) -> Self {
        Atom::String(value)
    }
}

impl From<&str> for Atom {
    fn from(value: &str) -> Self {
        Atom::String(value.to_owned())
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Int(val) => write!(f, "{}", val),
            Atom::Float(val) => write!(f, "{}", val),
            Atom::String(s) => write!(f, "\"{}\"", s),
            Atom::Symbol(name) => write!(f, "{}", name),
        }
    }
}

// --------------------------------------------------------------------------------------------

// for testing
fn get_symbol(value: &str) -> SExpr
{
    SExpr::Atom(Atom::Symbol(value.into()), 0, 0)
}

/// qulisp s-expression
#[derive(Debug, Clone)]
pub enum SExpr {
    List(Vec<Rc<SExpr>>, usize, usize),
    Atom(Atom, usize, usize),
    Pair(Rc<SExpr>, Rc<SExpr>, usize, usize),
}

impl From<Vec<SExpr>> for SExpr {
    fn from(value: Vec<SExpr>) -> Self {
        let value = value.into_iter().map(|x| Rc::new(x)).collect();
        SExpr::List(value, 0, 0)
    }
}

impl From<&[SExpr]> for SExpr {
    fn from(value: &[SExpr]) -> Self {
        let value = value.into_iter().map(|x| Rc::new(x.clone())).collect();
        SExpr::List(value, 0, 0)
    }
}

impl From<(SExpr, SExpr)> for SExpr {
    fn from(value: (SExpr, SExpr)) -> Self {
        SExpr::Pair(Rc::new(value.0), Rc::new(value.1), 0, 0)
    }
}

impl From<i64> for SExpr {
    fn from(value: i64) -> Self {
        SExpr::Atom(Atom::Int(value), 0, 0)
    }
}

impl From<f64> for SExpr {
    fn from(value: f64) -> Self {
        SExpr::Atom(Atom::Float(value), 0, 0)
    }
}

impl From<String> for SExpr {
    fn from(value: String) -> Self {
        SExpr::Atom(Atom::String(value), 0, 0)
    }
}

impl From<&str> for SExpr {
    fn from(value: &str) -> Self {
        SExpr::Atom(Atom::String(value.to_owned()), 0, 0)
    }
}

impl From<Symbol> for SExpr {
    fn from(value: Symbol) -> Self {
        SExpr::Atom(Atom::Symbol(value), 0, 0)
    }
}

impl PartialEq for SExpr {
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
            _ => false,
        }
    }
}

impl<'code> Display for SExpr {
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
        }
    }
}

// --------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use lalrpop_util::ParseError;
    use crate::ast::get_symbol;
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
                get_symbol("define"),
                vec![
                    get_symbol("square"),
                    get_symbol("x"),
                ].into(),
                vec![
                    get_symbol("*"),
                    get_symbol("x"),
                    get_symbol("x"),
                ].into(),
            ].into()),
            Rc::new(vec![
                get_symbol("define"),
                vec![
                    get_symbol("sum-of-squares"),
                    get_symbol("x"),
                    get_symbol("y"),
                ].into(),
                vec![
                    get_symbol("+"),
                    vec![get_symbol("square"), get_symbol("x")].into(),
                    vec![get_symbol("square"), get_symbol("y")].into(),
                ].into()
            ].into()),
            Rc::new(vec![
                get_symbol("sum-of-squares"),
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
                get_symbol("define"),
                get_symbol("print_msg"),
                vec![
                    get_symbol("msg"),
                    get_symbol("src"),
                    get_symbol("dst"),
                ].into(),
                vec![
                    get_symbol("print"),
                    "Message from ".into(),
                    get_symbol("src"),
                    " to ".into(),
                    get_symbol("dst"),
                    " : ".into(),
                    get_symbol("msg"),
                ].into(),
            ].into()),
            Rc::new(vec![
                get_symbol("print_msg"),
                "hello world".into(),
                "me".into(),
                "you".into(),
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