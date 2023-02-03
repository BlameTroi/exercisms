// Package stringset provides a simple set of strings.
package stringset

import (
	"fmt"
	"strings"
)

// Set is a set of strings.
type Set struct {
	kb map[string]bool
}

// New returns a new empty set.
func New() Set {
	return Set{kb: make(map[string]bool)}
}

// NewFromSlice creates a new Set initialized with the values from
// the slice of strings.
func NewFromSlice(i []string) Set {
	s := New()
	for _, e := range i {
		s.kb[e] = true
	}
	return s
}

// String returns a string representation of the Sets contents. Order is not
// guaranteed.
func (s Set) String() string {
	w := strings.Builder{}
	w.WriteRune('{')
	first := true
	for k := range s.kb {
		if !first {
			w.WriteString(", ")
		}
		w.WriteString(fmt.Sprintf("%q", k))
		first = false
	}
	w.WriteRune('}')
	return w.String()
}

// IsEmpty should be obvious.
func (s Set) IsEmpty() bool {
	return len(s.kb) == 0
}

// Has tests to see if a particular string is an element of the
// Set.
func (s Set) Has(i string) bool {
	return s.kb[i]
}

// Subset determines if s1 is a subset of s2.
//
// {a b c} subset {a b c} = true
// {a b c} subset {a b} = false
// {} subset {a b c} = true
// {} subset {} = true
func Subset(s1, s2 Set) bool {
	if len(s1.kb) == 0 && len(s2.kb) == 0 {
		return true
	}
	if len(s1.kb) == 0 {
		return true
	}
	if len(s2.kb) == 0 {
		return false
	}
	if len(s2.kb) < len(s1.kb) {
		return false
	}
	for k := range s1.kb {
		if !s2.Has(k) {
			return false
		}
	}
	return true
}

// Disjoint determines if the two sets have no
// common elements.
//
// {a b c} is disjoint from {d e f}
// {a b c} is not disjoint from {c d e f}
func Disjoint(s1, s2 Set) bool {
	if len(s1.kb) == 0 || len(s2.kb) == 0 {
		return true
	}
	for k := range s1.kb {
		if s2.Has(k) {
			return false
		}
	}
	for k := range s2.kb {
		if s1.Has(k) {
			return false
		}
	}
	return true
}

// Equal compares two Sets for equality.
func Equal(s1, s2 Set) bool {
	if len(s1.kb) != len(s2.kb) {
		return false
	}
	for k := range s1.kb {
		if !s2.Has(k) {
			return false
		}
	}
	for k := range s2.kb {
		if !s1.Has(k) {
			return false
		}
	}
	return true
}

// Add a new element to the set.
func (s Set) Add(i string) {
	s.kb[i] = true
}

// Intersection returns a Set of all the elements in
// both Sets.
//
// {} intersect {c b e} = {}
// {a b c} intersect {b d g} = {b}
func Intersection(s1, s2 Set) Set {
	s3 := New()
	for k := range s1.kb {
		if s2.Has(k) {
			s3.Add(k)
		}
	}
	for k := range s2.kb {
		if s1.Has(k) {
			s3.Add(k)
		}
	}
	return s3
}

// Difference (Complement) of two sets is the set of all
// elements only in the first set.
//
// {} difference {a b c} = {}
// {a} difference {a b c} = {}
// {a b} difference {b c d} = {b}
func Difference(s1, s2 Set) Set {
	s3 := New()
	for k := range s1.kb {
		if !s2.Has(k) {
			s3.Add(k)
		}
	}
	return s3
}

// Union creates a new Set with all the elements in
// both sets.
//
// Union({a, b, c}, {b, c, d}) = {a, b, c, d}
func Union(s1, s2 Set) Set {
	s3 := New()
	for k := range s1.kb {
		s3.Add(k)
	}
	for k := range s2.kb {
		s3.Add(k)
	}
	return s3
}
