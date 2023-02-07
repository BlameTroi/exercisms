// Package markdown provides a very limited markdown to html rendering.
package markdown

// Common utility functions and structs.1

import "strings"

// A stack for strings. There is no error handling by design, so
// peek(), push(), and pop() can panic. This is desirable for my
// usage.

type stack []string

func (s *stack) empty() bool   { return len(*s) == 0 }
func (s *stack) peek() string  { return (*s)[len(*s)-1] }
func (s *stack) push(i string) { (*s) = append((*s), i) }
func (s *stack) pop() string {
	d := (*s)[len(*s)-1]
	(*s) = (*s)[:len(*s)-1]
	return d
}

// Determine the length of a repeating run of the current
// rune in a Reader. Any error is treated as an EOF and
// returned to the caller along with the length of the run,
// if any. The Reader is left positioned immediately after
// the run finishes, which could be EOF.

func runLength(r *strings.Reader) (int, error) {
	r.UnreadRune()
	i := 1
	c, _, err := r.ReadRune()
	if err != nil {
		return 0, err
	}
	for {
		n, _, err := r.ReadRune()
		if err != nil {
			return i, err
		}
		if n != c {
			r.UnreadRune()
			return i, nil
		}
		i++
	}
}

// Skip forward to the first non blank character in a
// Reader. Any error is considered EOF and is returned
// to the caller.

func skipBlanks(r *strings.Reader) {
	for {
		c, _, err := r.ReadRune()
		if err != nil {
			return
		}
		if c == ' ' {
			continue
		}
		r.UnreadRune()
		return
	}
}

// Test for a supported markdown character which could signal
// the start of a markdown directive.

func isMarkdownRune(c rune) bool {
	return c == '#' || c == '*' || c == '_'
}
