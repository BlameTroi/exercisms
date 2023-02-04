// Package isogram evaluates words.
package isogram

import "strings"

// IsIsogram takes a string and determines if it is
// an isogram (no repeating letters, case insensitive).
func IsIsogram(s string) bool {
	s = strings.TrimSpace(s)
	// Zero or one letter is an isogram by definition.
	if len(s) < 2 {
		return true
	}
	s = strings.ToLower(s)
	m := make(map[rune]int)
	for _, c := range s {
		if c == '-' || c == ' ' {
			continue
		}
		m[c]++
		if m[c] != 1 {
			return false
		}
	}
	return true
}
