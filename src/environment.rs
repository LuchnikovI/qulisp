use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::IntoIterator;
use crate::ast::{SExpr, Exception, Atom};

// ------------------------------------------------------------------------------------------

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Environment<'code> {
    bindings: RefCell<HashMap<&'code str, Rc<SExpr<'code>>>>,
    parent: Option<Rc<Environment<'code>>>,
}

impl<'code> Environment<'code> {

    pub(super) fn new<N, V>(
        symbols: N,
        values: V,
    ) -> Rc<Environment<'code>>
    where
        N: IntoIterator<Item = &'code str>,
        V: IntoIterator<Item = Rc<SExpr<'code>>>,
    {
        let bindings: HashMap<_, _> = symbols
            .into_iter()
            .zip(values.into_iter())
            .collect();
        Rc::new(Environment {bindings: RefCell::new(bindings), parent: None})
    }

    pub(super) fn lookup(
        mut environment: Rc<Environment<'code>>,
        symbol: &'code str,
    ) -> Rc<SExpr<'code>>
    {
        loop {
            if let Some(var) = environment.bindings.borrow().get(symbol) {
                return var.clone()
            }
            let parent = if let Some(parent) = &environment.parent {
                parent.clone()
            } else {
                return Rc::new(SExpr::Exception(Exception::new(format!("Symbol '{symbol}' is unbind"))))
            };
            environment = parent;
        }
    }

    pub(super) fn extend<N, V>(
        environment: Rc<Environment<'code>>,
        symbols: N,
        values: V,
    ) -> Rc<Environment>
    where
        N: IntoIterator<Item = &'code str>,
        V: IntoIterator<Item = Rc<SExpr<'code>>>,
    {
        let bindings: HashMap<_, _> = symbols
            .into_iter()
            .zip(values.into_iter())
            .collect();
        let new_environment = Environment { bindings: RefCell::new(bindings), parent: Some(environment) };
        Rc::new(new_environment)
    }

    pub(super) fn bind(
        environment: Rc<Environment<'code>>,
        symbol: &'code str,
        value: Rc<SExpr<'code>>,
    )
    {
        environment.bindings.borrow_mut().insert(symbol, value);
    }

    pub(super) fn set(
        mut environment: Rc<Environment<'code>>,
        symbol: &'code str,
        value: Rc<SExpr<'code>>,
    ) -> Rc<SExpr<'code>>
    {
        loop {
            if let Some(var) = environment.bindings.borrow_mut().get_mut(symbol) {
                *var = value;
                return Rc::new(Atom::Nil.into());
            }
            let parent = if let Some(parent) = &environment.parent {
                parent.clone()
            } else {
                return Rc::new(SExpr::Exception(Exception::new(format!("Symbol '{symbol}' is unbind"))));
            };
            environment = parent;
        }
    }
}

// ------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use crate::{
        ast::{SExpr, Atom, Exception},
    };
    use super::Environment;

    #[test]
    fn test_small_branching_environment()
    {
        let environment = Environment::new(
            ["x", "foo", "bar"],
            [
                Rc::<SExpr>::new(1i64.into()).into(),
                Rc::<SExpr>::new(2i64.into()).into(),
                Rc::<SExpr>::new(42f64.into()).into(),
            ]
        );
        let mut environment1 = Environment::extend(
            environment.clone(),
            ["foo", "baz"],
            [
                Rc::<SExpr>::new(2i64.into()).into(),
                Rc::<SExpr>::new(28i64.into()).into(),
            ],
        );
        let lmbd: SExpr = vec![
            "quax".into(),
            vec!["arg1".into(), "arg2".into()].into(),
            environment1.clone().into(),
        ].into();
        Environment::bind(environment1.clone(), "lmbd", Rc::new(lmbd.clone()));
        environment1 = Environment::extend(
            environment1,
            ["==", "<=>"],
            [
                Rc::<SExpr>::new(2222i64.into()).into(),
                Rc::<SExpr>::new(2828i64.into()).into(),
            ],
        );
        let environment2 = Environment::extend(environment.clone(), [], []);
        Environment::bind(environment2.clone(), "x", Rc::<SExpr>::new(123i64.into()).into());
        Environment::bind(environment2.clone(), "x", Rc::<SExpr>::new(122i64.into()).into());
        assert_eq!(SExpr::Exception(Exception::new("Symbol 'a' is unbind".to_owned())), *Environment::lookup(environment2.clone(), "a"));
        assert_eq!(SExpr::Atom(Atom::Int(2), None), *Environment::lookup(environment2.clone(), "foo"));
        assert_eq!(SExpr::Atom(Atom::Int(122), None), *Environment::lookup(environment2.clone(), "x"));
        assert_eq!(lmbd, *Environment::lookup(environment1.clone(), "lmbd"));
        assert_eq!(SExpr::Atom(Atom::Nil, None), *Environment::set(environment1.clone(), "bar", Rc::<SExpr>::new("hello world!".to_string().into()).into()));
        assert_eq!(SExpr::Atom(Atom::String("hello world!".to_owned()), None), *Environment::lookup(environment2.clone(), "bar"))
    }
}