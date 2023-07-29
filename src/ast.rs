use std::fmt::Display;

/// Errors that may appear during grammar code snippets invocation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QLispParseError {

    /// Appears when one cannot parse a given int in code into rust i64
    CannotParseInt(usize, usize),

    /// Appears when one cannot parse a given float in code into rust f64
    CannotParseFloat(usize, usize),
}


/// quantum-lisp keywords
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyWord {
    Define,
    If,
    Let,
    Cond,
    Lambda,
}

impl Display for KeyWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeyWord::Define => write!(f, "define"),
            KeyWord::If => write!(f, "if"),
            KeyWord::Let => write!(f, "let"),
            KeyWord::Cond => write!(f, "cond"),
            KeyWord::Lambda => write!(f, "lambda"),
        }
    }
}

/// quantum-lisp atomic objects
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Atom<'code> {
    Int(i64),
    Float(f64),
    Symbol(&'code str),
    KeyWord(KeyWord),
}

impl<'code> Display for Atom<'code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Int(val) => write!(f, "{}", val),
            Atom::Float(val) => write!(f, "{}", val),
            Atom::Symbol(name) => write!(f, "{}", name),
            Atom::KeyWord(keyword) => write!(f, "{}", keyword),
        }
    }
}

/// quantum-lisp s-expression
#[derive(Debug, Clone, PartialEq)]
pub enum SExpr<'code> {
    List(Vec<SExpr<'code>>),
    Atom(Atom<'code>),
    Pair(Box<SExpr<'code>>, Box<SExpr<'code>>),
}

impl<'code> Display for SExpr<'code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::Atom(atom) => write!(f, "{}", atom),
            SExpr::List(list) => {
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
            SExpr::Pair(lhs, rhs) => write!(f, "({} . {})", lhs, rhs),
        }
    }
}

// ----------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use lalrpop_util::ParseError;
    use crate::grammar::ProgramParser;
    use std::str::from_utf8;

    #[test]
    fn test_define_fn_ast()
    {
        let code = "(   
            define   (sum-of-squares x  y)
                        (+ (square x   )
                         (square y  ) ))

                    (sum-of-squares 3.14e-3 40)";
        let ast = ProgramParser::new().parse(code).unwrap();
        assert_eq!(
            &format!("{}", ast[0]),
            "(define (sum-of-squares x y) (+ (square x) (square y)))"
        );
        assert_eq!(&format!("{}", ast[1]), "(sum-of-squares 0.00314 40)");
    }

    #[test]
    fn test_cond_ast()
    {
        let code = "(define (abs x)
                        (
                cond ( (   > x 
                    0
                ) x)
                              (
                (= 
            x 0) 0)
                              ((< x 0) (- x)))) (abs .99)";
        let ast = ProgramParser::new().parse(code).unwrap();
        assert_eq!(
            &format!("{}", ast[0]),
            "(define (abs x) (cond ((> x 0) x) ((= x 0) 0) ((< x 0) (- x))))"
        );
        assert_eq!(&format!("{}", ast[1]), "(abs 0.99)");
    }

    #[test]
    fn test_factorial_ast()
    {
        let code = "
        (define (factorial n-u-m-b-e-r)
            (if (
    = n-u-m-b-e-r 1) 1 (  * n ( factorial (  - n 1  )))))";
        let ast = ProgramParser::new().parse(code).unwrap();
        assert_eq!(
            &format!("{}", ast[0]),
            "(define (factorial n-u-m-b-e-r) (if (= n-u-m-b-e-r 1) 1 (* n (factorial (- n 1)))))"
        );
    }

    #[test]
    fn test_to_big_number_ast()
    {
        let code = "(   
            define   (sum-of-squares x  y)
                        (+ (square x   )
                         (square y  ) ))

                    (sum-of-squares 3.14e-3 400000000000000000000000000000000000000000000000000000)";
        let ast = ProgramParser::new().parse(code);
        match ast.unwrap_err() {
            ParseError::User { error } => {
                match error {
                    super::QLispParseError::CannotParseInt(start, end) => {
                        assert_eq!(
                            "400000000000000000000000000000000000000000000000000000",
                            from_utf8(&code.as_bytes()[start..end]).unwrap(),
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
}