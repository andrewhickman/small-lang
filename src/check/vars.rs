use crate::check::scheme::ReducedScheme;
use crate::syntax::{Symbol, SymbolMap};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub(crate) struct VarId(u32);

#[derive(Default)]
pub(crate) struct Vars {
    data: Vec<VarData>,
    ids: SymbolMap<Vec<VarId>>,
}

pub(crate) struct VarData {
    scheme: ReducedScheme,
}

impl Vars {
    pub fn push(&mut self, symbol: Symbol, scheme: ReducedScheme) -> VarId {
        let id = VarId(self.data.len() as u32);
        self.data.push(VarData { scheme });
        self.ids.entry(symbol).or_default().push(id);
        id
    }

    pub fn get(&mut self, symbol: Symbol) -> Option<&VarData> {
        let id = self.ids.entry(symbol).or_default().last().copied();
        match id {
            Some(VarId(id)) => Some(&self.data[id as usize]),
            None => None,
        }
    }

    pub fn pop(&mut self, symbol: Symbol) -> VarId {
        self.ids.get_mut(&symbol).unwrap().pop().unwrap()
    }
}

impl VarData {
    pub fn scheme(&self) -> &ReducedScheme {
        &self.scheme
    }
}
