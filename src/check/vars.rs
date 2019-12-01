use serde::Serialize;

use crate::check::scheme::ReducedScheme;
use crate::check::FileSpan;
use crate::syntax::{Symbol, SymbolMap};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize)]
pub struct VarId(u32);

#[derive(Default)]
pub(crate) struct Vars {
    data: Vec<VarData>,
    ids: SymbolMap<Vec<VarId>>,
}

pub(crate) struct VarData {
    pub name: Symbol,
    pub span: Option<FileSpan>,
    pub scheme: ReducedScheme,
    pub uses: u32,
}

impl Vars {
    pub fn next(&mut self) -> VarId {
        VarId(self.data.len() as u32)
    }

    pub fn push(&mut self, name: Symbol, span: Option<FileSpan>, scheme: ReducedScheme) -> VarId {
        let id = self.next();
        self.data.push(VarData {
            name,
            scheme,
            span,
            uses: 0,
        });
        self.ids.entry(name).or_default().push(id);
        id
    }

    pub fn get_id(&mut self, name: Symbol) -> Option<VarId> {
        self.ids.entry(name).or_default().last().copied()
    }

    pub fn get_mut(&mut self, id: VarId) -> &mut VarData {
        &mut self.data[id.0 as usize]
    }

    pub fn pop(&mut self, name: Symbol) -> (VarId, VarData) {
        let id = self.ids.get_mut(&name).unwrap().pop().unwrap();
        let data = self.data.pop().unwrap();
        debug_assert_eq!(id, self.next());
        (id, data)
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl VarId {
    #[cfg(test)]
    pub fn new(id: u32) -> Self {
        VarId(id)
    }

    pub const BUILTIN_EQ: Self = VarId(0);
    pub const BUILTIN_GET_ADD: Self = VarId(1);
    pub const BUILTIN_GET_SUB: Self = VarId(2);
    pub const NUM_BUILTINS: usize = 3;

    pub fn is_builtin(&self) -> bool {
        (self.0 as usize) < Self::NUM_BUILTINS
    }
}
