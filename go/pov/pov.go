// Package pov demonstrates some aspects of directed graphs.
//
// This was originally presented as a problem
// best solved in a slice, but the update of
// the exercise forced a dynamic pointer
// approach.
package pov

import (
	"fmt"
)

//
// API
//

// Tree is an node in a tree structure.
// It can be a root, a parent, or leaf
// node. These nodes have one possible
// parent node and any number of child
// nodes managed in a slice.
type Tree struct {
	tag      string
	parent   *Tree
	children []*Tree
}

// Value returns the identifing name or
// tag of a Tree node. I prefer to think
// of these as tags. They should be unique
// in any Tree but this is not enforced.
func (t *Tree) Value() string {
	return t.tag
}

// Children returns a slice with all
// the immediate child nodes of the
// current Tree node.
func (t *Tree) Children() []*Tree {
	return t.children
}

// New returns a fully populated parent
// Tree node and any children. Trees are
// created fully grown, with their
// structure described in the arguments
// to the actual Tree root. The arguments
// read like a lisp s-expression.
func New(t string, c ...*Tree) *Tree {
	n := &Tree{
		tag:      t,
		parent:   nil,
		children: []*Tree{},
	}
	if c != nil {
		for _, e := range c {
			e.parent = n
			n.children = append(n.children, e)
		}
	}
	return n
}

// FromPov finds a subtree in the Tree
// by tag Value, and returns the Tree as
// seen from the POV of that subtree.
// In essence, the tree is re-rooted from
// the tagged node.
func (t *Tree) FromPov(tag string) *Tree {

	// does the new root exist?
	if t.find(tag) == nil {
		return nil
	}

	// create and modify a copy of the
	// current tree
	n := t.clone().find(tag)

	// pulling the pov node up to the root,
	// dragging its relations along kicking
	// and screaming.
	t, p := n, n.parent // n = new root node
	n.parent = nil      // t = main node this pass
	for p != nil {      // p = t's parent, q = grandparent
		t.children = append(t.children, p) // the child becomes the parent
		p.children = remove(p.children, t) // the parent becomes the child
		q := p.parent                      // and on to the grandparent
		p.parent = t                       // as in grandparent becomes p
		t, p = p, q                        // and parent becomes t
	}

	return n
}

// PathTo returns the shortest route from
// one node to another. This works best
// if we twist the tree so that the from
// node is the root.
func (t *Tree) PathTo(fromTag string, toTag string) []string {

	// find these nodes, exit early if we don't
	var ft, tt *Tree
	if ft = t.find(fromTag); ft == nil {
		return nil
	}
	if tt = t.find(toTag); tt == nil {
		return nil
	}

	r := []string{}

	bn := t.clone().FromPov(fromTag)
	cn := bn
	nx := &Tree{}
	r = append(r, cn.tag)

	for cn != nil {
		nx = nil
		for _, ea := range cn.children {
			if ea.find(toTag) != nil {
				r = append(r, ea.tag)
				nx = ea
				break
			}
		}
		cn = nx // how to handle error?
	}

	return r
}

//
// Implementation support
//

// find reads down a Tree and returns the
// node named. If no match is found, nil is
// returned.
func (t *Tree) find(tag string) *Tree {
	if t.tag == tag {
		return t
	}
	for _, e := range t.children {
		x := e.find(tag)
		if x != nil {
			return x
		}
	}
	return nil
}

// Create a copy of the original tree.
// Tag relationships are preserved. This
// copy can be modified in place and
// returned by *Tree.FromPov(Value).
func (t *Tree) clone() *Tree {
	n := &Tree{t.tag, nil, []*Tree{}}
	if t.children == nil {
		return n
	}
	for _, e := range t.children {
		c := e.clone()
		c.parent = n
		n.children = append(n.children, c)
	}
	return n
}

// oneLine returns a formatted string
// displaying the tags of a node, its
// parent, and its immediate children,
// in a short display for easy tracing.
func (t *Tree) oneLine() string {
	p := " "
	if t.parent != nil {
		p = t.parent.tag
	}
	c := ""
	if len(t.children) != 0 {
		for _, e := range t.children {
			c = c + e.tag + " "
		}
		c = c[:len(c)-1]
	}
	return fmt.Sprintf("[%s %s (%s)]", t.tag, p, c)
}

// Dump out the Tree nodes recursively,
// one line per node with its children.
// An optional header string can be
// provided. This header is not passed
// into recurisve dumps.
func (t *Tree) dump(s ...string) {
	if s != nil {
		fmt.Println(s[0])
	}
	fmt.Println(t.oneLine())
	for _, e := range t.children {
		e.dump()
	}
}

// remove rather naively removes one
// element from the children of a
// Tree node. Yes, this can be done
// by slicing and dicing, but these
// are all pointers.
func remove(col []*Tree, mem *Tree) []*Tree {
	if col == nil || len(col) == 0 {
		return []*Tree{}
	}
	if mem == nil {
		return col
	}
	upd := make([]*Tree, 0, len(col)-1)
	for _, e := range col {
		if e != mem {
			upd = append(upd, e)
		}
	}
	return upd
}
