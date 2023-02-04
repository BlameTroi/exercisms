// Package tree is an exercise is creating a tree structure for bulletin board
// comments stored in random order in a database.
package tree

// Iterations:
// 1) golint and staticcheck are not complaining. I like most of the
//    labor division, but I want to reduce the number of sorts if I
//    can, and the drillDown() shouldn't include the records as a
//    parameter. Make it a closure in Build().

import (
	"errors"
	"sort"
)

// Function validate checks the records provided against the rules
// below. While error of nil would be sufficient to indicate that
// the records were valid, I prefer an explicit boolean. Use
// whichever you like.
//
// Rules for records in the dataset:
// * 0 <= ID < len(dataset)
// * ID > Parent for all records except root
// * Root is ID == Parent
// * no duplidate IDs
// * only one root
func validate(records []Record) (bool, error) {
	last, parent := -1, -1
	for _, r := range records {
		if r.ID >= len(records) || r.ID < 0 {
			return false, errors.New("record id out of range")
		}
		if last == r.ID {
			return false, errors.New("duplicate record id")
		}
		last = r.ID
		if r.ID == r.Parent {
			if parent != -1 {
				return false, errors.New("multiple root parents")
			}
			parent = r.ID
			continue
		}
		if r.ID < r.Parent {
			return false, errors.New("invalid parent reference")
		}
	}
	return true, nil
}

// Record represents the key and parent key information of bulletin
// board comments stored in a database. They could arrive in any
// any order.
type Record struct {
	ID     int
	Parent int
}

// RecordsByID implements sort.Interface on the ID.
type RecordsByID []Record

func (a RecordsByID) Len() int           { return len(a) }
func (a RecordsByID) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a RecordsByID) Less(i, j int) bool { return a[i].ID < a[j].ID }

// RecordsByParent implements the sort.Interface on the Parent&ID.
type RecordsByParent []Record

func (a RecordsByParent) Len() int      { return len(a) }
func (a RecordsByParent) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a RecordsByParent) Less(i, j int) bool {
	if a[i].Parent < a[j].Parent {
		return true
	}
	if a[i].Parent > a[j].Parent {
		return false
	}
	return a[i].ID < a[j].ID
}

// Node represents the tree structuring of Records.
type Node struct {
	ID       int
	Children []*Node
}

// Build takes a slice of Records and returns a tree of Nodes.
func Build(records []Record) (*Node, error) {
	// Guards:
	if len(records) == 0 {
		return nil, nil
	}

	// Validate the record set.
	sort.Sort(RecordsByID(records))
	if _, err := validate(records); err != nil {
		return nil, err
	}

	// Only one record? If so it must be a childless root (see
	// validate()). Return the one node tree.
	if len(records) == 1 {
		return &Node{ID: records[0].ID}, nil
	}

	// Find the root parent. One might assume that it would
	// be the first record after the sort and validation,
	// but that's not guaranteed. Children referencing a
	// parent that isn't in the dataset could be included.
	//
	// Any such children will not be added to any Node.
	parentAt := -1
	for i, r := range records {
		if r.ID == r.Parent {
			parentAt = i
			break
		}
	}

	// Error back if no parent. This should have been
	// caught in validate() but double checking is cheap
	// and could avoid a panic or loop later.
	if parentAt == -1 {
		return nil, errors.New("no root parent found")
	}

	// Build parent with no children.
	root := Node{ID: records[parentAt].ID}

	// Sort on Parent&ID and then build the tree
	// recursively.
	sort.Sort(RecordsByParent(records))
	drillDown(&root, records)
	// todo: more to come
	return &root, nil
}

// Function drillDown finds all the children in the records for
// a given node, and then recursively calls itself for any
// children found.
//
// There are a few optimizations that could be done, but I
// don't think they will provide meaningful benefit at this
// time.
func drillDown(curr *Node, records []Record) {
	for _, r := range records {
		if r.Parent == curr.ID && r.ID != r.Parent {
			curr.Children = append(curr.Children, &Node{ID: r.ID})
		}
	}
	if len(curr.Children) != 0 {
		for _, c := range curr.Children {
			drillDown(c, records)
		}
	}
}
