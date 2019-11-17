use std::cell::RefCell;
use std::rc::Rc;

use im::OrdMap;
use mlsub::auto::StateSet;
use mlsub::Polarity;

use crate::check::ty::NumberConstructor;
use crate::syntax::Symbol;

#[derive(Default)]
pub(in crate::check) struct Capabilities {
    empty_pos: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    empty_neg: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    int_pos: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    int_neg: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    float_pos: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    float_neg: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    string_pos: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    string_neg: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
}

impl Capabilities {
    pub fn set_empty(&self, pos: OrdMap<Symbol, StateSet>, neg: OrdMap<Symbol, StateSet>) {
        *self.empty_pos.borrow_mut() = Some(pos);
        *self.empty_neg.borrow_mut() = Some(neg);
    }

    pub fn set_int(&self, pos: OrdMap<Symbol, StateSet>, neg: OrdMap<Symbol, StateSet>) {
        *self.int_pos.borrow_mut() = Some(pos);
        *self.int_neg.borrow_mut() = Some(neg);
    }

    pub fn set_float(&self, pos: OrdMap<Symbol, StateSet>, neg: OrdMap<Symbol, StateSet>) {
        *self.float_pos.borrow_mut() = Some(pos);
        *self.float_neg.borrow_mut() = Some(neg);
    }

    pub fn set_string(&self, pos: OrdMap<Symbol, StateSet>, neg: OrdMap<Symbol, StateSet>) {
        *self.string_pos.borrow_mut() = Some(pos);
        *self.string_neg.borrow_mut() = Some(neg);
    }

    pub fn empty(&self, pol: Polarity) -> Rc<RefCell<Option<im::OrdMap<Symbol, StateSet>>>> {
        match pol {
            Polarity::Pos => self.empty_pos.clone(),
            Polarity::Neg => self.empty_neg.clone(),
        }
    }

    pub fn number(
        &self,
        pol: Polarity,
        num: NumberConstructor,
    ) -> Rc<RefCell<Option<im::OrdMap<Symbol, StateSet>>>> {
        match (pol, num) {
            (Polarity::Pos, NumberConstructor::Float) => self.float_pos.clone(),
            (Polarity::Neg, NumberConstructor::Float) => self.float_neg.clone(),
            (Polarity::Pos, NumberConstructor::Int) => self.int_pos.clone(),
            (Polarity::Neg, NumberConstructor::Int) => self.int_neg.clone(),
        }
    }

    pub fn string(&self, pol: Polarity) -> Rc<RefCell<Option<im::OrdMap<Symbol, StateSet>>>> {
        match pol {
            Polarity::Pos => self.string_pos.clone(),
            Polarity::Neg => self.string_neg.clone(),
        }
    }
}
