use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::hash::BuildHasherDefault;
use std::sync::{RwLock, RwLockWriteGuard};

use lazy_static::lazy_static;
use seahash::SeaHasher;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(u32);

pub type SymbolMap<V> = HashMap<Symbol, V, BuildHasherDefault<SeaHasher>>;
pub type ImSymbolMap<V> = im::HashMap<Symbol, V, BuildHasherDefault<SeaHasher>>;

impl Symbol {
    pub fn new(string: impl AsRef<str>) -> Self {
        Interner::write().intern(string.as_ref())
    }

    pub fn as_str(self) -> &'static str {
        INTERNER.read().unwrap().strings[self.0 as usize]
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("Symbol")
            .field(&self.0)
            .field(&self.as_str())
            .finish()
    }
}

impl serde::Serialize for Symbol {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_ref())
    }
}

impl<'de> serde::Deserialize<'de> for Symbol {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: Cow<str> = serde::Deserialize::deserialize(deserializer)?;
        Ok(Symbol::new(s))
    }
}

#[derive(Default)]
pub(crate) struct Interner {
    symbols: HashMap<&'static str, Symbol, BuildHasherDefault<SeaHasher>>,
    strings: Vec<&'static str>,
}

lazy_static! {
    static ref INTERNER: RwLock<Interner> = RwLock::default();
}

impl Interner {
    pub fn write<'a>() -> RwLockWriteGuard<'a, Self> {
        INTERNER.write().unwrap()
    }

    pub fn intern(&mut self, string: &str) -> Symbol {
        if let Some(&symbol) = self.symbols.get(string) {
            return symbol;
        }

        let symbol = Symbol(self.strings.len() as u32);
        let string = Box::leak(string.into());
        self.symbols.insert(string, symbol);
        self.strings.push(string);
        symbol
    }
}
