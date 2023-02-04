// Package binarysearchtree demonstrates building
// and walking a bst.
package binarysearchtree

// SearchTreeData is a node in the bst.
type SearchTreeData struct {
	data        int
	left, right *SearchTreeData
}

// Bst creates a single unlinked node in the bst.
func Bst(i int) *SearchTreeData {
	return &SearchTreeData{data: i}
}

// Insert adds an item to the bst. If a node with
// the same data value already exists, the tree is
// not modified but this is not an error.
func (bst *SearchTreeData) Insert(i int) {
	curr := bst
	for {
		if i <= curr.data {
			if curr.left != nil {
				curr = curr.left
				continue
			}
			curr.left = Bst(i)
			return
		} else {
			if curr.right != nil {
				curr = curr.right
				continue
			}
			curr.right = Bst(i)
			return
		}
	}
}

// MapString walks the bst and applies a function to
// the data in the tree and returns a slice of the
// results of f in the correct order.
func (bst *SearchTreeData) MapString(f func(int) string) []string {
	res := []string{}
	if bst.left != nil {
		res = bst.left.MapString(f)
	}
	res = append(res, f(bst.data))
	if bst.right != nil {
		res = append(res, bst.right.MapString(f)...)
	}
	return res
}

// MapInt walks the bst and applies a function to
// the data in the tree and returns a slice of the
// results of f in the correct order.
func (bst *SearchTreeData) MapInt(f func(int) int) []int {
	res := []int{}
	if bst.left != nil {
		res = bst.left.MapInt(f)
	}
	res = append(res, f(bst.data))
	if bst.right != nil {
		res = append(res, bst.right.MapInt(f)...)
	}
	return res
}
