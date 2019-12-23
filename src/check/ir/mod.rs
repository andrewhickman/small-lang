mod visitor;

pub use self::visitor::Visitor;

use std::collections::BTreeMap;
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use crate::check::VarId;
use crate::rt::{Command, Value};
use crate::syntax::Symbol;
use crate::FileSpan;

#[derive(Debug)]
pub struct Expr<T = Rc<[Command]>> {
    pub nodes: Nodes<T>,
    pub id: NodeId,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd, serde::Serialize)]
pub struct NodeId(u32);

#[derive(Debug)]
pub struct Nodes<T = Rc<[Command]>> {
    data: Vec<Option<Node<T>>>,
}

#[derive(Debug)]
pub enum Node<T = Rc<[Command]>> {
    Literal(Value),
    Var(VarId),
    Call(Call),
    Let(Let),
    Func(Func),
    If(If),
    Proj(Proj),
    Enum(Enum),
    Record(BTreeMap<Symbol, NodeId>),
    Match(Match),
    Import(T),
    Ref(NodeId),
}

#[derive(Copy, Clone, Debug)]
pub struct Func {
    pub span: FileSpan,
    pub body: NodeId,
    pub rec_var: Option<VarId>,
}

#[derive(Copy, Clone, Debug)]
pub struct Call {
    pub arg: NodeId,
    pub func: NodeId,
}

#[derive(Copy, Clone, Debug)]
pub struct Let {
    pub val: NodeId,
    pub body: NodeId,
}

#[derive(Copy, Clone, Debug)]
pub struct If {
    pub cond: NodeId,
    pub cons: NodeId,
    pub alt: NodeId,
}

#[derive(Copy, Clone, Debug)]
pub struct Proj {
    pub expr: NodeId,
    pub field: Symbol,
}

#[derive(Copy, Clone, Debug)]
pub struct Enum {
    pub tag: Symbol,
    pub expr: NodeId,
}

#[derive(Clone, Debug)]
pub struct Match {
    pub expr: NodeId,
    pub cases: BTreeMap<Symbol, NodeId>,
}

impl<T> Nodes<T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn next(&mut self) -> NodeId {
        let id = NodeId(self.data.len() as u32);
        self.data.push(None);
        id
    }

    pub fn add_at(&mut self, id: NodeId, node: Node<T>) -> NodeId {
        let slot = &mut self.data[id.0 as usize];
        debug_assert!(slot.is_none());
        *slot = Some(node);
        id
    }

    pub fn add(&mut self, node: Node<T>) -> NodeId {
        let id = self.next();
        self.add_at(id, node);
        id
    }

    pub fn visit<F>(&self, node: NodeId, f: F)
    where
        F: FnMut(&Self, NodeId),
    {
        struct FnVisitor<'a, T, F> {
            nodes: &'a Nodes<T>,
            f: F,
        }

        impl<'a, T, F> Visitor<T> for FnVisitor<'a, T, F>
        where
            F: FnMut(&Nodes<T>, NodeId),
        {
            fn visit_node(&mut self, id: NodeId) {
                self.visit_expr(id, &self.nodes[id]);
                (self.f)(self.nodes, id);
            }
        }

        FnVisitor { f, nodes: self }.visit_node(node)
    }

    pub fn visit_mut<F>(&mut self, node: NodeId, mut f: F)
    where
        F: FnMut(&mut Self, NodeId),
    {
        // TODO proper mutable visitors
        let mut ids = Vec::new();
        self.visit(node, |_, id| ids.push(id));

        for id in ids {
            f(self, id);
        }
    }

    pub fn deref_id(&self, mut id: NodeId) -> NodeId {
        while let Some(Node::Ref(ref_id)) = self.data[id.0 as usize] {
            id = ref_id
        }
        id
    }
}

impl<T> Default for Nodes<T> {
    fn default() -> Self {
        Nodes {
            data: Default::default(),
        }
    }
}

impl<T> Index<NodeId> for Nodes<T> {
    type Output = Node<T>;

    fn index(&self, id: NodeId) -> &Self::Output {
        // let id = self.deref_id(id);
        self.data[id.0 as usize].as_ref().unwrap()
    }
}

impl<T> IndexMut<NodeId> for Nodes<T> {
    fn index_mut(&mut self, id: NodeId) -> &mut Self::Output {
        // let id = self.deref_id(id);
        self.data[id.0 as usize].as_mut().unwrap()
    }
}

impl NodeId {
    pub const fn builtin(id: u32) -> Self {
        NodeId(!id)
    }

    pub fn get(self) -> u32 {
        self.0
    }
}

#[cfg(test)]
impl<T: PartialEq> PartialEq for Expr<T> {
    fn eq(&self, other: &Self) -> bool {
        use std::collections::HashSet;

        use itertools::{EitherOrBoth, Itertools};

        fn eq<T: PartialEq>(
            vars: &mut HashSet<(VarId, VarId)>,
            lhs_nodes: &Nodes<T>,
            rhs_nodes: &Nodes<T>,
            lhs: NodeId,
            rhs: NodeId,
        ) -> bool {
            vars.insert((lhs, rhs));
            match (&lhs_nodes[lhs], &rhs_nodes[rhs]) {
                (Node::Literal(l), Node::Literal(r)) => l == r,
                (Node::Var(l), Node::Var(r)) => vars.contains(&(*l, *r)),
                (Node::Call(l), Node::Call(r)) => {
                    eq(vars, lhs_nodes, rhs_nodes, l.arg, r.arg)
                        && eq(vars, lhs_nodes, rhs_nodes, l.func, r.func)
                }
                (Node::Let(l), Node::Let(r)) => {
                    eq(vars, lhs_nodes, rhs_nodes, l.val, r.val)
                        && eq(vars, lhs_nodes, rhs_nodes, l.body, r.body)
                }
                (Node::Func(l), Node::Func(r)) => {
                    l.rec_var.is_some() == r.rec_var.is_some()
                        && eq(vars, lhs_nodes, rhs_nodes, l.body, r.body)
                }
                (Node::If(l), Node::If(r)) => {
                    eq(vars, lhs_nodes, rhs_nodes, l.cond, r.cond)
                        && eq(vars, lhs_nodes, rhs_nodes, l.cons, r.cons)
                        && eq(vars, lhs_nodes, rhs_nodes, l.alt, r.alt)
                }
                (Node::Proj(l), Node::Proj(r)) => {
                    l.field == r.field && eq(vars, lhs_nodes, rhs_nodes, l.expr, r.expr)
                }
                (Node::Enum(l), Node::Enum(r)) => {
                    l.tag == r.tag && eq(vars, lhs_nodes, rhs_nodes, l.expr, r.expr)
                }
                (Node::Record(l), Node::Record(r)) => {
                    l.iter().zip_longest(r.iter()).all(|eob| match eob {
                        EitherOrBoth::Both(l, r) => {
                            l.0 == r.0 && eq(vars, lhs_nodes, rhs_nodes, *l.1, *r.1)
                        }
                        _ => false,
                    })
                }
                (Node::Match(l), Node::Match(r)) => {
                    eq(vars, lhs_nodes, rhs_nodes, l.expr, r.expr)
                        && l.cases
                            .iter()
                            .zip_longest(r.cases.iter())
                            .all(|eob| match eob {
                                EitherOrBoth::Both(l, r) => {
                                    l.0 == r.0 && eq(vars, lhs_nodes, rhs_nodes, *l.1, *r.1)
                                }
                                _ => false,
                            })
                }
                (Node::Import(l), Node::Import(r)) => l == r,
                (Node::Ref(lhs), _) => eq(vars, lhs_nodes, rhs_nodes, *lhs, rhs),
                (_, Node::Ref(rhs)) => eq(vars, lhs_nodes, rhs_nodes, lhs, *rhs),
                _ => false,
            }
        }

        eq(
            &mut HashSet::new(),
            &self.nodes,
            &other.nodes,
            self.id,
            other.id,
        )
    }
}

impl<T> Node<T> {
    pub fn is_decl(&self) -> bool {
        match self {
            Node::Let(_) | Node::Func(_) | Node::Match(_) => true,
            _ => false,
        }
    }
}
