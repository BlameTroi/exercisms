// Package acronym creates acronyms from phrases via
// simple abbreviation.
package acronym

import (
	"strings"
)

// The Abbreviate function takes a string of one or
// more words and returns an acronym of the initial
// letter of each word. Punctuation and other non-
// alphabetic characters are ignored.
func Abbreviate(s string) string {
	// Clean up the input string by normalizing case
	// and stripping out most non-alphabetic characters.
	// Apostrophes and periods are preserved.
	cleaner := func(r rune) rune {
		switch {
		case 'A' <= r && r <= 'Z':
			return r
		case 'a' <= r && r <= 'z':
			return r - ('a' - 'A')
		case r == '\'' || r == '.':
			return r
		default:
			return ' '
		}
	}
	cleaned := strings.Map(cleaner, s)
	// Combine first letters of each word.
	words := strings.Split(cleaned, " ")
	var result strings.Builder
	for _, w := range words {
		if len(w) != 0 {
			result.WriteRune(rune(w[0]))
		}
	}
	return result.String()
}
