use std::cell::RefCell;
use std::rc::Rc;

use im::OrdMap;
use mlsub::auto::StateSet;

use crate::check::ty::NumberConstructor;
use crate::syntax::Symbol;

pub(in crate::check) struct Capabilities {
    empty: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    int: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    float: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    string: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
}

impl Default for Capabilities {
    fn default() -> Self {
        Capabilities {
            empty: Rc::new(RefCell::new(Some(OrdMap::default()))),
            int: Default::default(),
            float: Default::default(),
            string: Default::default(),
        }
    }
}

impl Capabilities {
    pub fn set_int(&self, capabilities: OrdMap<Symbol, StateSet>) {
        *self.int.borrow_mut() = Some(capabilities);
    }

    pub fn set_float(&self, capabilities: OrdMap<Symbol, StateSet>) {
        *self.float.borrow_mut() = Some(capabilities);
    }

    pub fn set_string(&self, capabilities: OrdMap<Symbol, StateSet>) {
        *self.string.borrow_mut() = Some(capabilities);
    }

    pub fn empty(&self) -> Rc<RefCell<Option<im::OrdMap<Symbol, StateSet>>>> {
        self.empty.clone()
    }

    pub fn number(
        &self,
        num: NumberConstructor,
    ) -> Rc<RefCell<Option<im::OrdMap<Symbol, StateSet>>>> {
        match num {
            NumberConstructor::Float => self.float.clone(),
            NumberConstructor::Int => self.int.clone(),
        }
    }

    pub fn string(&self) -> Rc<RefCell<Option<im::OrdMap<Symbol, StateSet>>>> {
        self.string.clone()
    }
}
