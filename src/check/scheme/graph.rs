use std::collections::HashMap;
use std::iter::FromIterator;
use std::string::ToString;

use mlsub::auto::StateId;
use mlsub::{ConstructorSet, Label as _, Polarity};
use serde::ser::{SerializeSeq, SerializeStruct, Serializer};
use serde::Serialize;
use smallvec::SmallVec;

use crate::check::scheme::ReducedScheme;
use crate::check::ty::{Constructor, Label};
use crate::syntax::Symbol;

pub struct Graph {
    pub nodes: HashMap<StateId, Node>,
    pub edges: Vec<(EdgeKind, StateId, StateId)>,
}

pub struct Node {
    pub pol: Polarity,
    pub kind: NodeKind,
    pub constructors: ConstructorSet<Constructor>,
}

pub enum NodeKind {
    Root,
    Env(Symbol),
    Other,
}

pub enum EdgeKind {
    Flow,
    Label(Label),
}

impl ReducedScheme {
    pub fn to_graph(&self) -> Graph {
        let mut stack = Vec::with_capacity(1 + self.scheme.env.len());
        stack.extend(
            self.scheme
                .env
                .iter()
                .map(|(&var, &id)| (id, Polarity::Neg, NodeKind::Env(var))),
        );
        stack.push((self.scheme.ty, Polarity::Pos, NodeKind::Root));

        let mut nodes = HashMap::with_capacity(stack.len());
        let mut edges = Vec::with_capacity(stack.len());

        while let Some((id, pol, kind)) = stack.pop() {
            nodes.insert(
                id,
                Node {
                    pol,
                    kind,
                    constructors: self.auto[id].constructors().clone(),
                },
            );

            for con in self.auto[id].constructors().iter() {
                con.visit_params(|label, ids| {
                    for sink_id in ids {
                        if !nodes.contains_key(&sink_id) {
                            stack.push((sink_id, pol * label.polarity(), NodeKind::Other));
                        }

                        edges.push((EdgeKind::Label(label), id, sink_id));
                    }
                });
            }

            if pol == Polarity::Neg {
                for sink_id in self.auto[id].flow().iter() {
                    if !nodes.contains_key(&sink_id) {
                        stack.push((sink_id, -pol, NodeKind::Other));
                    }

                    edges.push((EdgeKind::Flow, id, sink_id));
                }
            }
        }

        Graph { nodes, edges }
    }
}

// https://github.com/dagrejs/graphlib/wiki/API-Reference#json-write
impl Serialize for Graph {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut ser_struct = serializer.serialize_struct("graph", 4)?;
        ser_struct.serialize_field("value", &SerializeGraphValue { label: "scheme" })?;
        ser_struct.serialize_field(
            "options",
            &SerializeOptions {
                directed: true,
                compound: false,
                multigraph: true,
            },
        )?;
        ser_struct.serialize_field("nodes", &SerializeNodes { data: &self.nodes })?;
        ser_struct.serialize_field("edges", &SerializeEdges { data: &self.edges })?;
        ser_struct.end()
    }
}

#[derive(Serialize)]
struct SerializeGraphValue {
    label: &'static str,
}

#[derive(Serialize)]
struct SerializeOptions {
    directed: bool,
    compound: bool,
    multigraph: bool,
}

struct SerializeNodes<'a> {
    data: &'a HashMap<StateId, Node>,
}

#[derive(Serialize)]
struct SerializeNode {
    #[serde(rename = "v")]
    id: String,
    value: SerializeNodeValue,
}

#[derive(Serialize)]
struct SerializeNodeValue {
    label: &'static str,
    polarity: &'static str,
    components: SmallVec<[&'static str; 1]>,
}

struct SerializeEdges<'a> {
    data: &'a [(EdgeKind, StateId, StateId)],
}

#[derive(Serialize)]
struct SerializeEdge {
    #[serde(rename = "v")]
    source: String,
    #[serde(rename = "w")]
    sink: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    value: SerializeEdgeValue,
}

#[derive(Serialize)]
struct SerializeEdgeValue {
    label: &'static str,
}

impl<'a> Serialize for SerializeNodes<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut ser_seq = serializer.serialize_seq(Some(self.data.len()))?;
        for (id, node) in self.data {
            let id = id.as_u32().to_string();
            let label = match node.kind {
                NodeKind::Root => "root",
                NodeKind::Env(symbol) => symbol.as_str(),
                NodeKind::Other => "",
            };
            let polarity = match node.pol {
                Polarity::Pos => "pos",
                Polarity::Neg => "neg",
            };

            ser_seq.serialize_element(&SerializeNode {
                id,
                value: SerializeNodeValue {
                    label,
                    polarity,
                    components: SmallVec::from_iter(
                        node.constructors
                            .iter()
                            .map(|constructor| constructor.short_name()),
                    ),
                },
            })?;
        }
        ser_seq.end()
    }
}

impl<'a> Serialize for SerializeEdges<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut ser_seq = serializer.serialize_seq(Some(self.data.len()))?;
        for (kind, source, sink) in self.data {
            let name = match kind {
                EdgeKind::Flow => None,
                EdgeKind::Label(label) => Some(label.to_string()),
            };

            ser_seq.serialize_element(&SerializeEdge {
                source: source.as_u32().to_string(),
                sink: sink.as_u32().to_string(),
                name,
                value: SerializeEdgeValue { label: "" },
            })?;
        }
        ser_seq.end()
    }
}
