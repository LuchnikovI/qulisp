use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::IntoIterator;
use crate::ast::SExpr;

// ------------------------------------------------------------------------------------------

#[derive(Debug, Default, Clone)]
pub struct Environment<'code> {
    bindings: RefCell<HashMap<&'code str, Rc<SExpr<'code>>>>,
    parent: Option<Rc<Environment<'code>>>,
}

// Assume that environments are equal if and only if they are the same environment
impl<'code> PartialEq for Environment<'code>
{
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'code> Environment<'code> {

    pub(super) fn new(
        symbols: impl IntoIterator<Item = Rc<SExpr<'code>>>,
        values: impl IntoIterator<Item = Rc<SExpr<'code>>>,
    ) -> Rc<SExpr<'code>>
    {
        let mut bindings = HashMap::new();
        for (symbol, value) in symbols.into_iter().zip(values) {
            match symbol.try_symbol() {
                Ok(s) => {
                    if let Some(_) = bindings.insert(s, value) {
                        return Rc::new(SExpr::exception(format!("Rebinding of a symbol {s}"), symbol.code_position));
                    }
                },
                Err(e) => return Rc::new(e),
            }
        }
        Rc::new(SExpr::env(Rc::new(Environment {bindings: RefCell::new(bindings), parent: None})))
    }

    pub(super) fn lookup(
        environment: Rc<SExpr<'code>>,
        symbol: Rc<SExpr<'code>>,
    ) -> Rc<SExpr<'code>>
    {
        let sy = match symbol.try_symbol() {
            Ok(sy) => sy,
            Err(e) => return Rc::new(e),
        };
        let mut environment = match environment.try_env() {
            Ok(e) => e,
            Err(e) => return Rc::new(e),
        };
        loop {
            if let Some(var) = environment.bindings.borrow().get(sy) {
                return var.clone()
            }
            let parent = if let Some(parent) = &environment.parent {
                parent.clone()
            } else {
                return Rc::new(SExpr::exception(format!("Symbol '{sy}' is unbind"), symbol.code_position))
            };
            environment = parent;
        }
    }

    pub(super) fn extend(
        environment: Rc<SExpr<'code>>,
        symbols: impl IntoIterator<Item = Rc<SExpr<'code>>>,
        values: impl IntoIterator<Item = Rc<SExpr<'code>>>,
    ) -> Rc<SExpr<'code>>
    {
        let environment = match environment.try_env() {
            Ok(e) => e,
            Err(e) => return Rc::new(e),
        };
        let mut bindings = HashMap::new();
        for (symbol, value) in symbols.into_iter().zip(values) {
            match symbol.try_symbol() {
                Ok(s) => {
                    if let Some(_) = bindings.insert(s, value) {
                        return Rc::new(SExpr::exception(format!("Rebinding of a symbol {s}"), symbol.code_position));
                    }
                },
                Err(e) => return Rc::new(e),
            }
        }
        let new_environment = SExpr::env(
            Rc::new(Environment {bindings: RefCell::new(bindings), parent: Some(environment)})
        );
        return Rc::new(new_environment);
    }

    pub(super) fn bind(
        environment: Rc<SExpr<'code>>,
        symbol: Rc<SExpr<'code>>,
        value: Rc<SExpr<'code>>,
    ) -> Rc<SExpr<'code>>
    {
        let sy = match symbol.try_symbol() {
            Ok(sy) => sy,
            Err(e) => return Rc::new(e),
        };
        let environment = match environment.try_env() {
            Ok(e) => e,
            Err(e) => return Rc::new(e),
        };
        environment.bindings.borrow_mut().insert(sy, value);
        Rc::new(SExpr::nil(None))
    }

    pub(super) fn set(
        environment: Rc<SExpr<'code>>,
        symbol: Rc<SExpr<'code>>,
        value: Rc<SExpr<'code>>,
    ) -> Rc<SExpr<'code>>
    {
        let sy = match symbol.try_symbol() {
            Ok(sy) => sy,
            Err(e) => return Rc::new(e),
        };
        let mut environment = match environment.try_env() {
            Ok(e) => e,
            Err(e) => return Rc::new(e),
        };
        loop {
            if let Some(var) = environment.bindings.borrow_mut().get_mut(sy) {
                *var = value;
                return Rc::new(SExpr::nil(None));
            }
            let parent = if let Some(parent) = &environment.parent {
                parent.clone()
            } else {
                return Rc::new(SExpr::exception(format!("Symbol '{sy}' is unbind"), symbol.code_position))
            };
            environment = parent;
        }
    }
}

// ------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use crate::ast::SExpr;
    use super::Environment;

    #[test]
    fn test_small_branching_environment()
    {
        let environment = Environment::new(
            [
                Rc::new(SExpr::symbol("x", None)),
                Rc::new(SExpr::symbol("foo", None)),
                Rc::new(SExpr::symbol("bar", None))
            ],
            [
                Rc::new(SExpr::int(1i64, None)),
                Rc::new(SExpr::int(2i64, None)),
                Rc::new(SExpr::float(42f64, None)),
            ]
        );
        let mut environment1 = Environment::extend(
            environment.clone(),
            [
                Rc::new(SExpr::symbol("foo", None)),
                Rc::new(SExpr::symbol("baz", None)),
            ],
            [
                Rc::new(SExpr::int(2i64, None)),
                Rc::new(SExpr::int(28i64, None)),
            ],
        );
        let lmbd = Rc::new(SExpr::list(vec![
            SExpr::symbol("quax", None),
            SExpr::list(vec![SExpr::symbol("arg1", None), SExpr::symbol("arg2", None)], None),
            (*environment1).clone(),
        ], None));
        Environment::bind(
            environment1.clone(),
            Rc::new(SExpr::symbol("lmbd", None)),
            lmbd.clone(),
        );
        environment1 = Environment::extend(
            environment1,
            [
                Rc::new(SExpr::symbol("==", None)),
                Rc::new(SExpr::symbol("<=>", None)),
            ],
            [
                Rc::new(SExpr::int(2222i64.into(), None)),
                Rc::new(SExpr::int(2828i64.into(), None)),
            ],
        );
        let environment2 = Environment::extend(environment.clone(), [], []);
        Environment::bind(
            environment2.clone(),
            Rc::new(SExpr::symbol("x", None)),
            Rc::new(SExpr::int(123i64, None)),
        );
        Environment::bind(
            environment2.clone(),
            Rc::new(SExpr::symbol("x", None)),
            Rc::new(SExpr::int(122i64, None)),
        );
        assert_eq!(
            SExpr::exception("Symbol 'a' is unbind".to_owned(), None),
            *Environment::lookup(environment2.clone(), Rc::new(SExpr::symbol("a", None))),
        );
        assert_eq!(
            SExpr::int(2, None),
            *Environment::lookup(environment2.clone(), Rc::new(SExpr::symbol("foo", None))),
        );
        assert_eq!(
            SExpr::int(122, None),
            *Environment::lookup(environment2.clone(), Rc::new(SExpr::symbol("x", None))),
        );
        assert_eq!(
            lmbd,
            Environment::lookup(environment1.clone(), Rc::new(SExpr::symbol("lmbd", None))),
        );
        assert_eq!(
            SExpr::nil(None),
            *Environment::set(
                environment1.clone(),
                Rc::new(SExpr::symbol("bar", None)),
                Rc::new(SExpr::string("hello world!".to_string(), None)),
            ),
        );
        assert_eq!(
            Rc::new(SExpr::string("hello world!".to_string(), None)),
            Environment::lookup(environment2.clone(), Rc::new(SExpr::symbol("bar", None))),
        )
    }
}