// Package brackets provides some simple syntax validation.
package brackets

// Bracket determines if the various brackets in a string are
// properly balanced.
func Bracket(i string) bool {
	s := NewStack()

	// closure to check if top of stack holds the expected rune
	matches := func(r rune) bool {
		if s.Empty() {
			return false
		}
		return r == s.Pop()
	}

	// iterate over the input, place an expected close bracket on
	// the stack each time an open bracket is seen. when a close
	// is seen, it must match the current top of the stack.
	for _, r := range i {
		switch r {
		case '(':
			s.Push(')')
		case '[':
			s.Push(']')
		case '{':
			s.Push('}')
		case ')':
			if !matches(r) {
				return false
			}
		case ']':
			if !matches(r) {
				return false
			}
		case '}':
			if !matches(r) {
				return false
			}
		}
	}

	// stack should be empty if the brakcets are balanced.
	return s.Empty()
}

// Type Stack provides a simple Stack implementation.
type Stack []rune

func NewStack() Stack        { return make(Stack, 0, 8) }
func (s *Stack) Empty() bool { return len(*s) == 0 }
func (s *Stack) Peek() rune  { return (*s)[len(*s)-1] }
func (s *Stack) Push(i rune) { (*s) = append((*s), i) }
func (s *Stack) Pop() rune {
	d := (*s)[len(*s)-1]
	(*s) = (*s)[:len(*s)-1]
	return d
}
