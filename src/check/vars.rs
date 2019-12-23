use std::collections::HashMap;

use crate::check::ir;
use crate::check::scheme::ReducedScheme;
use crate::syntax::{Symbol, SymbolMap};
use crate::FileSpan;

pub type VarId = ir::NodeId;

#[derive(Default)]
pub(crate) struct Vars {
    data: HashMap<VarId, VarData>,
    ids: SymbolMap<Vec<VarId>>,
}

pub(crate) struct VarData {
    pub name: Symbol,
    pub span: Option<FileSpan>,
    pub scheme: ReducedScheme,
    pub uses: u32,
}

impl Vars {
    pub fn push(&mut self, id: VarId, name: Symbol, span: Option<FileSpan>, scheme: ReducedScheme) {
        self.data.insert(
            id,
            VarData {
                name,
                scheme,
                span,
                uses: 0,
            },
        );
        self.ids.entry(name).or_default().push(id);
    }

    pub fn get_id(&mut self, name: Symbol) -> Option<VarId> {
        self.ids.entry(name).or_default().last().copied()
    }

    pub fn get(&self, id: VarId) -> &VarData {
        self.data.get(&id).unwrap()
    }

    pub fn get_mut(&mut self, id: VarId) -> &mut VarData {
        self.data.get_mut(&id).unwrap()
    }

    pub fn pop(&mut self, name: Symbol) -> VarId {
        self.ids.get_mut(&name).unwrap().pop().unwrap()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl VarId {
    pub const BUILTIN_EQ: Self = ir::NodeId::builtin(0);
    pub const BUILTIN_GET_ADD: Self = ir::NodeId::builtin(1);
    pub const BUILTIN_GET_SUB: Self = ir::NodeId::builtin(2);
    pub const NUM_BUILTINS: usize = 3;

    pub fn is_builtin(self) -> bool {
        self.as_builtin() < VarId::NUM_BUILTINS
    }

    pub fn as_builtin(self) -> usize {
        !self.get() as usize
    }
}
