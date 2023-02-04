// Package linkedlist provies a basic doubly linked list that
// can hold a variety of values thanks to the use of interface{}.
package linkedlist

import "errors"

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
// func NewList(args ...interface{}) *List
//
// func (l *List) PushFront(v interface{})
// from instructions unshift insert value at front
//
// func (l *List) PushBack(v interface{})
// from instructions push insert value at back
//
// func (l *List) PopFront() (interface{}, error)
// from instructions shift remove value at front
//
// func (l *List) PopBack() (interface{}, error)
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
	Val        interface{}
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
		n := &Node{Val: v, next: nil, prev: prev}
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
func (l *List) PushFront(v interface{}) {
	if l.front == nil {
		l.front = &Node{Val: v}
		l.back = l.front
		return

	}
	next := l.front
	l.front = &Node{Val: v, next: next}
	next.prev = l.front
}

// PUshFront adds a value to the end of the List.
func (l *List) PushBack(v interface{}) {
	if l.back == nil {
		l.back = &Node{Val: v}
		l.front = l.back
		return
	}
	prev := l.back
	l.back = &Node{Val: v, prev: prev}
	prev.next = l.back
}

// PopFront removes the value from the front of the List.
func (l *List) PopFront() (interface{}, error) {
	if l.front == nil {
		return new(interface{}), ErrEmptyList
	}
	res := l.front.Val
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
func (l *List) PopBack() (interface{}, error) {
	if l.back == nil {
		return new(interface{}), ErrEmptyList
	}
	res := l.back.Val
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
		args = append(args, curr.Val)
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
		v, _ := l.PopBack()
		args = append(args, v)
	}
	for _, v := range args {
		l.PushBack(v)
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
