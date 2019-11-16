use std::cell::RefCell;
use std::rc::Rc;

use im::OrdMap;
use mlsub::auto::StateSet;

use crate::syntax::Symbol;

#[derive(Default)]
pub(in crate::check) struct Capabilities {
    empty: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
}

impl Capabilities {
    pub fn set_empty(&self, capabilities: OrdMap<Symbol, StateSet>) {
        *self.empty.borrow_mut() = Some(capabilities);
    }

    pub fn empty(&self) -> Rc<RefCell<Option<im::OrdMap<Symbol, StateSet>>>> {
        self.empty.clone()
    }
}
