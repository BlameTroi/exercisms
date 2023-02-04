// Package pov demonstrates some aspects of directed graphs.
package pov

// Graph is a set of nodes having parent to child relationships.
type Graph struct {
	nodes []node
}

// node describes a node in the graph. Any node can have exactly
// one parent. Any graph, once completely built, should have only
// one parent node.
//
// Unfortunately, an empty parent pointer/index into the graph's
// nodes is -1.
type node struct {
	label  string
	parent int
}

// New returns an empty graph.
func New() *Graph {
	return &Graph{}
}

// AddNode adds a new node to the graph. The node
// should be a leaf node and no parent is set.
func (g *Graph) AddNode(label string) {
	if g.find(label) == -1 {
		g.add(label)
	}
}

// AddArc creates a parent -> child relationship
// in the graph. The child node should exist, but
// the parent might not and it will be added if
// needed.
func (g *Graph) AddArc(parent, child string) {
	np, nc := g.find(parent), g.find(child)
	if np == -1 {
		np = g.add(parent)
	}
	g.nodes[nc].parent = np
}

// ArcList returns a list of all the parent to child
// relationships in the graph.
func (g *Graph) ArcList() []string {
	res := []string{}
	for _, n := range g.nodes {
		if n.parent != -1 {
			res = append(res, g.nodes[n.parent].label+" -> "+n.label)
		}
	}
	return res
}

// ChangeRoot pivots or inverts the graph so that it has a
// new root node by reversing some of the parent->child
// relationships between nodes. A original graph is left
// unchanged and a new graph is returned if any changes
// are made.
func (g *Graph) ChangeRoot(old, new string) *Graph {

	// Clone the graph.
	ng := &Graph{}
	ng.nodes = append(ng.nodes, g.nodes...)

	// Don't bother if there are no nodes, no change,
	// or the requested roots are not in the tree.
	if len(ng.nodes) <= 1 || old == new {
		return ng
	}
	oldRoot, newRoot := ng.find(old), ng.find(new)
	if oldRoot == -1 || newRoot == -1 {
		return ng
	}

	// walk from new root to old root, flipping the
	// relationships
	// todo: collapse into one pass
	path := []int{}
	next := newRoot
	for next != -1 {
		path = append(path, next)
		next = g.nodes[next].parent
	}
	prior := -1
	for _, i := range path {
		ng.nodes[i].parent = prior
		prior = i
	}

	return ng
}

// find a node by label and returns its index
// or -1 for not found.
func (g *Graph) find(label string) int {
	for i, n := range g.nodes {
		if n.label == label {
			return i
		}
	}
	return -1
}

// Add a node by label and return its index. if
// a node by that label already exists, return
// its index.
func (g *Graph) add(label string) int {
	n := g.find(label)
	if n != -1 {
		return n
	}
	g.nodes = append(g.nodes, node{label: label, parent: -1})
	return len(g.nodes) - 1
}
