use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::iter::IntoIterator;
use crate::ast::SExpr;

// ------------------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EnvError<'sexpr> {
    UnbindVariable(&'sexpr str)
}

type EnvResult<'sexpr, T> = Result<T, EnvError<'sexpr>>;

// ------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Variable<'sexpr> {
    SExpr(Rc<SExpr>),
    Lambda(&'sexpr [String], Rc<SExpr>, Weak<Frame<'sexpr>>),
}

impl<'sexpr> From<Rc<SExpr>> for Variable<'sexpr> {
    fn from(value: Rc<SExpr>) -> Self {
        Variable::SExpr(value)
    }
}

// ------------------------------------------------------------------------------------------

#[derive(Debug, Default, Clone)]
pub(super) struct Frame<'sexpr> {
    bindings: RefCell<HashMap<&'sexpr str, Variable<'sexpr>>>,
    parent: Option<Rc<Frame<'sexpr>>>,
}

impl<'sexpr> Frame<'sexpr> {

    fn new<N, V>(
        names: N,
        values: V,
    ) -> Rc<Frame<'sexpr>>
    where
        N: IntoIterator<Item = &'sexpr str>,
        V: IntoIterator<Item = Variable<'sexpr>>,
    {
        let bindings: HashMap<_, _> = names
            .into_iter()
            .zip(values.into_iter())
            .collect();
        Rc::new(Frame {bindings: RefCell::new(bindings), parent: None})
    }

    fn lookup(
        mut frame: Rc<Frame<'sexpr>>,
        name: &'sexpr str
    ) -> EnvResult<'sexpr, Variable<'sexpr>>
    {
        loop {
            if let Some(var) = frame.bindings.borrow().get(name) {
                return Ok(var.clone());
            }
            let parent = if let Some(parent) = &frame.parent {
                parent.clone()
            } else {
                return Err(EnvError::UnbindVariable(name));
            };
            frame = parent;
        }
    }

    fn extend<N, V>(
        frame: Rc<Frame<'sexpr>>,
        names: N,
        values: V,
    ) -> Rc<Frame<'sexpr>>
    where
        N: IntoIterator<Item = &'sexpr str>,
        V: IntoIterator<Item = Variable<'sexpr>>,
    {
        let bindings: HashMap<_, _> = names
            .into_iter()
            .zip(values.into_iter())
            .collect();
        let new_frame = Frame { bindings: RefCell::new(bindings), parent: Some(frame) };
        Rc::new(new_frame)
    }

    fn bind(
        frame: Rc<Frame<'sexpr>>,
        name: &'sexpr str,
        value: Variable<'sexpr>,
    )
    {
        frame.bindings.borrow_mut().insert(name, value);
    }

    fn set(
        mut frame: Rc<Frame<'sexpr>>,
        name: &'sexpr str,
        value: Variable<'sexpr>,
    ) -> EnvResult<'sexpr, ()>
    {
        loop {
            if let Some(var) = frame.bindings.borrow_mut().get_mut(name) {
                *var = value;
                return Ok(());
            }
            let parent = if let Some(parent) = &frame.parent {
                parent.clone()
            } else {
                return Err(EnvError::UnbindVariable(name));
            };
            frame = parent;
        }
    }
}

// ------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::rc::{Rc, Weak};
    use crate::{
        ast::{SExpr, Atom},
        environment::{EnvError, Variable},
        grammar::ProgramParser
    };
    use super::Frame;

    #[test]
    fn test_small_branching_environment()
    {
        let frame = Frame::new(
            ["x", "foo", "bar"],
            [
                Rc::<SExpr>::new(1i64.into()).into(),
                Rc::<SExpr>::new(2i64.into()).into(),
                Rc::<SExpr>::new(42f64.into()).into(),
            ]
        );
        let mut frame1 = Frame::extend(
            frame.clone(),
            ["foo", "baz", "lmbd"],
            [
                Rc::<SExpr>::new(2i64.into()).into(),
                Rc::<SExpr>::new(28i64.into()).into(),
            ],
        );
        let args = vec!["arg1".to_owned(), "arg2".to_owned()];
        let body = ProgramParser::new().parse("(* (+ arg1 arg2) 11)").unwrap();
        let lmbd = Variable::Lambda(
            args.as_slice(),
            body[0].clone(),
            Rc::downgrade(&frame1)
        );
        Frame::bind(frame1.clone(), "lmbd", lmbd);
        frame1 = Frame::extend(
            frame1,
            ["==", "<=>"],
            [
                Rc::<SExpr>::new(2222i64.into()).into(),
                Rc::<SExpr>::new(2828i64.into()).into(),
            ],
        );
        let frame2 = Frame::extend(frame.clone(), [], []);
        Frame::bind(frame2.clone(), "x", Rc::<SExpr>::new(123i64.into()).into());
        Frame::bind(frame2.clone(), "x", Rc::<SExpr>::new(122i64.into()).into());
        assert_eq!(EnvError::UnbindVariable("a"), Frame::lookup(frame2.clone(), "a").unwrap_err());
        if let Variable::SExpr(sexpr) = Frame::lookup(frame2.clone(), "foo").unwrap() {
            assert_eq!(sexpr, Rc::new(SExpr::Atom(Atom::Int(2), 0, 0)))
        } else {
            panic!("Variable must be expression not a lambda")
        };
        if let Variable::SExpr(sexpr) = Frame::lookup(frame2.clone(), "x").unwrap() {
            assert_eq!(sexpr, Rc::new(SExpr::Atom(Atom::Int(122), 0, 0)))
        } else {
            panic!("Variable must be expression not a lambda")
        };
        if let Variable::Lambda(a, b, w) = Frame::lookup(frame1.clone(), "lmbd").unwrap() {
            assert_eq!(args, a);
            assert_eq!(body[0].clone(), b);
            let _ = Weak::upgrade(&w).unwrap();
        } else {
            panic!("Variable must be lambda not an expression");
        }
        Frame::set(frame1.clone(), "bar", Rc::<SExpr>::new("hello world!".into()).into()).unwrap();
        if let Variable::SExpr(sexpr) = Frame::lookup(frame2.clone(), "bar").unwrap() {
            assert_eq!(sexpr, Rc::new(SExpr::Atom(Atom::String("hello world!".to_owned()), 0, 0)))
        } else {
            panic!("Variable must be expression not a lambda")
        };
    }
}