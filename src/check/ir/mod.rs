mod visitor;

pub use self::visitor::Visitor;

use std::collections::BTreeMap;
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use crate::check::VarId;
use crate::rt::{Command, Value};
use crate::syntax::Symbol;

#[derive(Debug)]
pub struct Expr<T = Rc<[Command]>> {
    pub nodes: Nodes<T>,
    pub id: NodeId,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NodeId(u32);

#[derive(Debug)]
pub struct Nodes<T = Rc<[Command]>> {
    data: Vec<Node<T>>,
}

#[derive(Clone, Debug)]
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
}

#[derive(Copy, Clone, Debug)]
pub struct Func {
    pub arg: VarId,
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
    pub name: VarId,
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
    pub cases: BTreeMap<Symbol, MatchCase>,
}

#[derive(Copy, Clone, Debug)]
pub struct MatchCase {
    pub expr: NodeId,
    pub name: Option<VarId>,
}

impl<T> Nodes<T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add(&mut self, node: Node<T>) -> NodeId {
        let id = NodeId(self.data.len() as u32);
        self.data.push(node);
        id
    }

    pub fn iter<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Self, NodeId),
    {
        for i in 0..self.data.len() {
            f(self, NodeId(i as u32));
        }
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
        &self.data[id.0 as usize]
    }
}

impl<T> IndexMut<NodeId> for Nodes<T> {
    fn index_mut(&mut self, id: NodeId) -> &mut Self::Output {
        &mut self.data[id.0 as usize]
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
            match (&lhs_nodes[lhs], &rhs_nodes[rhs]) {
                (Node::Literal(l), Node::Literal(r)) => l == r,
                (Node::Var(l), Node::Var(r)) => vars.contains(&(*l, *r)),
                (Node::Call(l), Node::Call(r)) => {
                    eq(vars, lhs_nodes, rhs_nodes, l.arg, r.arg)
                        && eq(vars, lhs_nodes, rhs_nodes, l.func, r.func)
                }
                (Node::Let(l), Node::Let(r)) => {
                    vars.insert((l.name, r.name))
                        && eq(vars, lhs_nodes, rhs_nodes, l.val, r.val)
                        && eq(vars, lhs_nodes, rhs_nodes, l.body, r.body)
                }
                (Node::Func(l), Node::Func(r)) => {
                    vars.insert((l.arg, r.arg))
                        && l.rec_var.is_some() == r.rec_var.is_some()
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
                                    l.0 == r.0
                                        && match ((l.1).name, (r.1).name) {
                                            (Some(l), Some(r)) => vars.insert((l, r)),
                                            (None, None) => true,
                                            _ => false,
                                        }
                                        && eq(vars, lhs_nodes, rhs_nodes, (l.1).expr, (r.1).expr)
                                }
                                _ => false,
                            })
                }
                (Node::Import(l), Node::Import(r)) => l == r,
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
