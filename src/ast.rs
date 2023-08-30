use std::rc::Rc;
use std::fmt::Display;
use crate::builtins::{TRUE, NIL, QUOTE};
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

/// Exception structure to keep errors during interpretation (it is also s-expr)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exception {
    msg: String,
    code_positions: Vec<(usize, usize)>,
}

// --------------------------------------------------------------------------------------------

/// qulisp s-expression
#[derive(Debug, Clone, PartialEq)]
pub(super) enum SExprBody<'code> {
    List(Rc<[Rc<SExpr<'code>>]>),
    Pair(Rc<SExpr<'code>>, Rc<SExpr<'code>>),
    Int(i64),
    Float(f64),
    Symbol(&'code str),
    String(Rc<String>),
    Nil,
    True,
    Exception(Rc<Exception>),
    Environment(Rc<Environment<'code>>),
}

#[derive(Debug, Clone)]
pub struct SExpr<'code> {
    body: SExprBody<'code>,
    pub code_position: Option<(usize, usize)>,
}

impl<'code> PartialEq for SExpr<'code> {

    fn eq(&self, other: &Self) -> bool {
        self.body.eq(&other.body)
    }

}

impl<'code> Display for SExpr<'code> {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.body {
            SExprBody::List(list) => {
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
            SExprBody::Pair(lhs, rhs) => write!(f, "({} . {})", lhs, rhs),
            SExprBody::Int(i) => write!(f, "{i}"),
            SExprBody::Float(fl) => write!(f, "{fl}"),
            SExprBody::Symbol(sy) => write!(f, "{sy}"),
            SExprBody::String(st) => write!(f, "{st}"),
            SExprBody::True => write!(f, "{TRUE}"),
            SExprBody::Nil => write!(f, "{NIL}"),
            SExprBody::Environment(_) => todo!(),
            SExprBody::Exception(_) => todo!(),
        }
    }

}

impl<'code> SExpr<'code> {

    #[inline]
    pub(super) fn get_type(&self) -> &'static str
    {
        match self.body {
            SExprBody::Int(_) => "integer",
            SExprBody::Float(_) => "float",
            SExprBody::Symbol(_) => "symbol",
            SExprBody::String(_) => "string",
            SExprBody::True => TRUE,
            SExprBody::Nil => NIL,
            SExprBody::List(_) => "list",
            SExprBody::Pair(..) => "pair",
            SExprBody::Environment(_) => "environment",
            SExprBody::Exception(_) => "exception",
        }
    }

    #[inline]
    pub(super) fn int(body: i64, code_position: Option<(usize, usize)>) -> Self
    {
        let body = SExprBody::Int(body);
        SExpr { body, code_position }
    }

    #[inline]
    pub(super) fn float(body: f64, code_position: Option<(usize, usize)>) -> Self
    {
        let body = SExprBody::Float(body);
        SExpr { body, code_position }
    }

    #[inline]
    pub(super) fn symbol(body: &'code str, code_position: Option<(usize, usize)>) -> Self
    {
        let body = SExprBody::Symbol(body);
        SExpr { body, code_position }
    }

    #[inline]
    pub(super) fn string(body: String, code_position: Option<(usize, usize)>) -> Self
    {
        let body = SExprBody::String(Rc::new(body));
        SExpr { body, code_position }
    }

    #[inline]
    pub(super) fn tru(code_position: Option<(usize, usize)>) -> Self
    {
        SExpr { body: SExprBody::True, code_position }
    }

    #[inline]
    pub(super) fn nil(code_position: Option<(usize, usize)>) -> Self
    {
        SExpr { body: SExprBody::Nil, code_position }
    }

    #[inline]
    pub(super) fn list(
        sexprs: impl IntoIterator<Item = Self>,
        code_position: Option<(usize, usize)>,
    ) -> Self
    {
        let raw_body: Rc<[_]> = sexprs.into_iter().map(|x| Rc::new(x)).collect::<Vec<_>>().into();
        let body = SExprBody::List(raw_body);
        SExpr { body, code_position }
    }

    #[inline]
    pub(super) fn pair(
        lhs: Self,
        rhs: Self,
        code_position: Option<(usize, usize)>,
    ) -> Self
    {
        let body = SExprBody::Pair(Rc::new(lhs), Rc::new(rhs));
        SExpr { body, code_position }
    }

    #[inline]
    pub(super) fn env(e: Rc<Environment<'code>>) -> Self
    {
        let body = SExprBody::Environment(e);
        SExpr { body, code_position: None }
    }

    #[inline]
    pub(super) fn exception(
        msg: String,
        code_position: Option<(usize, usize)>,
    ) -> Self
    {
        let body = SExprBody::Exception(
            Rc::new(Exception { msg, code_positions: vec![] })
        );
        SExpr { body, code_position }
    }

    #[inline]
    pub(super) fn type_exception(
        &self,
        what_expected: &str,
    ) -> Self
    {
        let sexpr_type = self.get_type();
        let code_position = self.code_position;
        let msg = format!("Expected s-expression of type {what_expected}, but got {sexpr_type}");
        SExpr::exception(msg, code_position)
    }

    #[inline]
    pub(super) fn try_symbol(&self) -> Result<&'code str, Self>
    {
        if let SExprBody::Symbol(sy) = self.body {
            Ok(sy)
        } else {
            Err(self.type_exception("symbol"))
        }
    }

    #[inline]
    pub(super) fn try_string(&self) -> Result<Rc<String>, Self>
    {
        if let SExprBody::String(st) = &self.body {
            Ok(st.clone())
        } else {
            Err(self.type_exception("string"))
        }
    }

    #[inline]
    pub(super) fn try_list(&self) -> Result<Rc<[Rc<Self>]>, Self>
    {
        if let SExprBody::List(l) = &self.body {
            Ok(l.clone())
        } else {
            Err(self.type_exception("list"))
        }
    }

    #[inline]
    pub(super) fn try_pair(
        &self,
    ) -> Result<(Rc<Self>, Rc<Self>), Self>
    {
        if let SExprBody::Pair(lhs, rhs) = &self.body {
            Ok((lhs.clone(), rhs.clone()))
        } else {
            Err(self.type_exception("pair"))
        }
    }

    #[inline]
    pub(super) fn try_int(
        &self,
    ) -> Result<i64, Self>
    {
        if let SExprBody::Int(i) = self.body {
            Ok(i)
        } else {
            Err(self.type_exception("integer"))
        }
    }

    #[inline]
    pub(super) fn try_float(
        &self,
    ) -> Result<f64, Self>
    {
        if let SExprBody::Float(f) = self.body {
            Ok(f)
        } else {
            Err(self.type_exception("float"))
        }
    }

    #[inline]
    pub(crate) fn try_env(&self) -> Result<Rc<Environment<'code>>, Self>
    {
        if let SExprBody::Environment(e) = &self.body {
            Ok(e.clone())
        } else {
            Err(self.type_exception("environment"))
        }
    }

    #[inline]
    pub(super) fn is_true(
        &self,
    ) -> bool
    {
        if let SExprBody::Nil = self.body {
            false
        } else {
            true
        }
    }

    #[inline]
    pub(super) fn quote(&self, outher_code_position: Option<(usize, usize)>) -> Self
    {
        SExpr::list(vec![SExpr::symbol(QUOTE, None), self.clone()], outher_code_position)
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
            SExpr::list(vec![
                SExpr::symbol("define", None),
                SExpr::list(vec![
                    SExpr::symbol("square", None),
                    SExpr::symbol("x", None),
                ], None),
                SExpr::list(vec![
                    SExpr::symbol("*", None),
                    SExpr::symbol("x", None),
                    SExpr::symbol("x", None),
                ], None),
            ], None),
            SExpr::list(vec![
                SExpr::symbol("define", None),
                SExpr::list(vec![
                    SExpr::symbol("sum-of-squares", None),
                    SExpr::symbol("x", None),
                    SExpr::symbol("y", None),
                ], None),
                SExpr::list(vec![
                    SExpr::symbol("+", None),
                    SExpr::list(vec![SExpr::symbol("square", None), SExpr::symbol("x", None)], None),
                    SExpr::list(vec![SExpr::symbol("square", None), SExpr::symbol("y", None)], None),
                ], None),
            ], None),
            SExpr::list(vec![
                SExpr::symbol("sum-of-squares", None),
                SExpr::float(0.00314f64, None),
                SExpr::int(40i64, None),
            ], None),
        ].into_iter().map(|x| Rc::new(x)).collect::<Vec<_>>();
        assert_eq!(ast, correct_ast);
    }

    #[test]
    fn test_print_many_strings()
    {
        let ast = ProgramParser::new().parse(PRINT_MSG).unwrap();
        let correct_ast: Vec<Rc<SExpr>> = vec![
            SExpr::list(vec![
                SExpr::symbol("define", None),
                SExpr::symbol("print_msg", None),
                SExpr::list(vec![
                    SExpr::symbol("msg", None),
                    SExpr::symbol("src", None),
                    SExpr::symbol("dst", None),
                ], None),
                SExpr::list(vec![
                    SExpr::symbol("print", None),
                    SExpr::string("Message from ".to_string(), None),
                    SExpr::symbol("src", None),
                    SExpr::string(" to ".to_string(), None),
                    SExpr::symbol("dst", None),
                    SExpr::string(" : ".to_string(), None),
                    SExpr::symbol("msg", None),
                ], None),
            ], None),
            SExpr::list(vec![
                SExpr::symbol("print_msg", None),
                SExpr::string("hello world".to_string(), None),
                SExpr::string("me".to_string(), None),
                SExpr::string("you".to_string(), None),
            ], None),
        ].into_iter().map(|x| Rc::new(x)).collect::<Vec<_>>();
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