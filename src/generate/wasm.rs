use std::rc::Rc;

use parity_wasm::elements::Module;

use crate::check::ir;

pub fn generate(_expr: &ir::Expr<Rc<Module>>) -> Module {
    unimplemented!()
}
