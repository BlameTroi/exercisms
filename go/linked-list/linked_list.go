// Package linkedlist provies a basic doubly linked list that
// can hold a variety of values thanks to the use of interface{}.
package linkedlist

import "errors"

// Update February 2023. The exercise was updated on Exercism
// with the API documentation being put in the README file.
// The APi was also changed with one rename and two new
// methods. Leaving the original here, see README.md for the
// final API.
//
// API as documented in the tests, with the functions listed in
// the instructions on the exercism site.
//
// type Node struct
// type List struct
// var ErrEmptyList
//
// func (e *Node) Next() *Node
// func (e *Node) Prev() *Node
//
// func (e *Node) Value interface{} ... was Val
//
// func NewList(args ...interface{}) *List
//
// func (l *List) PushFront(v interface{})
// renamed Unshift
// from instructions unshift insert value at front
//
// func (l *List) PushBack(v interface{})
// renamed Push
// from instructions push insert value at back
//
// func (l *List) PopFront() (interface{}, error)
// renamed Shift
// from instructions shift remove value at front
//
// func (l *List) PopBack() (interface{}, error)
// renamed Pop
// from instructions pop remove value at back
//
// func (l *List) Reverse() *List
//
// func (l *List) First() *Node
// func (l *List) Last() *Node
//
// The instructions claim that the tests will not cover
// error conditions, pop (PopBack) and shift (PopFront)
// will never be called on an empty list. This is a lie.

// Node is a note in the List. It has one exported
// variable.
type Node struct {
	Value      interface{}
	prev, next *Node
}

// List is a doublly linked list anchor.
type List struct {
	front, back *Node
}

var ErrEmptyList = errors.New("empty list")

// Next returns the address of the next Node.
func (e *Node) Next() *Node {
	if e == nil {
		return nil
	}
	return e.next
}

// Prev returns the address of the next Node.
func (e *Node) Prev() *Node {
	if e == nil {
		return nil
	}
	return e.prev
}

// NewList creates a new List from a slice of
// interface{} values.
func NewList(args ...interface{}) *List {
	res := &List{}
	var prev *Node
	for _, v := range args {
		n := &Node{Value: v, next: nil, prev: prev}
		if prev == nil {
			res.front = n
			res.back = n
		} else {
			prev.next = n
			n.prev = prev
			res.back = n
		}
		prev = n
	}
	return res
}

// PushFront adds a new value to the head of the List.
func (l *List) Unshift(v interface{}) {
	if l.front == nil {
		l.front = &Node{Value: v}
		l.back = l.front
		return

	}
	next := l.front
	l.front = &Node{Value: v, next: next}
	next.prev = l.front
}

// PushBack adds a value to the end of the List.
func (l *List) Push(v interface{}) {
	if l.back == nil {
		l.back = &Node{Value: v}
		l.front = l.back
		return
	}
	prev := l.back
	l.back = &Node{Value: v, prev: prev}
	prev.next = l.back
}

// PopFront removes the value from the front of the List.
func (l *List) Shift() (interface{}, error) {
	if l.front == nil {
		return new(interface{}), ErrEmptyList
	}
	res := l.front.Value
	next := l.front.next
	if next == nil {
		l.back = nil
	} else {
		next.prev = nil
	}
	l.front = next
	return res, nil
}

// PopBack removes the value from the end of the List.
func (l *List) Pop() (interface{}, error) {
	if l.back == nil {
		return new(interface{}), ErrEmptyList
	}
	res := l.back.Value
	prev := l.back.prev
	if prev == nil {
		l.front = nil
	} else {
		prev.next = nil
	}
	l.back = prev
	return res, nil
}

// CopyReverse is how Reverse should have been written
// based on the API doc in the tests. The code in the
// tests does not match the spec, see Reverse below.
func (l *List) CopyReverse() *List {
	args := []interface{}{}
	curr := l.back
	for {
		if curr == nil {
			break
		}
		args = append(args, curr.Value)
		curr = curr.prev
	}
	return NewList(args...)
}

// Reverse reorders a List in place efficiently by
// adjusting pointers.
func (l *List) Reverse() {
	n := l.front
	for n != nil {
		n.prev, n.next = n.next, n.prev
		n = n.prev
	}
	l.front, l.back = l.back, l.front
}

// Reverse reorders a List in place inefficiently.
func (l *List) NaiveReverse() {
	args := []interface{}{}
	for l.Last() != nil {
		v, _ := l.Pop()
		args = append(args, v)
	}
	for _, v := range args {
		l.Push(v)
	}
}

// First returns the first Node (not value) from the List.
func (l *List) First() *Node {
	return l.front
}

// Last returns the last Node (not value) from the List.
func (l *List) Last() *Node {
	return l.back
}
