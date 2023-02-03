package linkedlist

import "errors"

type Element struct {
	data int
	next *Element
}

type List struct {
	head *Element
	size int
}

func New(init []int) *List {
	if len(init) == 0 {
		return &List{}
	}
	l := &List{size: len(init)}
	var p *Element
	for _, d := range init {
		e := &Element{data: d}
		if l.head == nil {
			l.head = e
		} else {
			p.next = e
		}
		p = e
	}
	return l
}

func (l *List) Size() int { return l.size }

func (l *List) Push(d int) {
	n := &Element{data: d}
	if l.head == nil {
		l.size++
		l.head = n
		return
	}
	e := l.head
	for e.next != nil {
		e = e.next
	}
	e.next = n
	l.size++
}

func (l *List) Pop() (int, error) {
	if l.size == 0 {
		return 0, errors.New("can't pop empty list")
	}
	var p *Element
	e := l.head
	for e.next != nil {
		p = e
		e = e.next
	}
	if p != nil {
		p.next = nil
	}
	l.size--
	return e.data, nil
}

func (l *List) Array() []int {
	if l.size == 0 {
		return []int{}
	}
	a := make([]int, l.size)
	i := 0
	e := l.head
	for e != nil {
		a[i] = e.data
		i++
		e = e.next
	}
	return a
}

func (l *List) Reverse() *List {
	if l.size == 0 {
		return &List{}
	}
	s := l.Array()
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
	return New(s)
}
