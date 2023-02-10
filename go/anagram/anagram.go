// Package anagram is a coding exercise.
package anagram

import (
	"sort"
	"strings"
)

// Detect returns a list of anagrams of
// SUBJECT from the list of CANDIDATES.
// A word is not its own anagram, regardless
// of letter case, but case is otherwise
// not significant.
func Detect(subject string, candidates []string) []string {
	r := []string{}
	for _, e := range candidates {
		if areAnagrams(subject, e) {
			r = append(r, e)
		}
	}
	return r
}

// areAnagrams determines if the two
// words passed are anagrams according
// to our rules:
//   - A word is never its own anagram,
//     regardless of case.
//   - Case is otherwise not significant.
func areAnagrams(s, t string) bool {
	if strings.ToLower(s) == strings.ToLower(t) {
		return false
	}
	if len(s) != len(t) {
		return false
	}
	return normalize(s) == normalize(t)
}

// normalized takes a unicode string and
// returns a string appropriate to the
// case insensitive anagram check:
// * case folded to lower case
// * split into runes (not characters)
// * runes sorted into ascending order
// * runes concatenated into a string
func normalize(s string) string {
	rs := strings.NewReader(strings.ToLower(s))
	buff := make([]rune, 0, rs.Size())
	for rs.Len() > 0 {
		ch, _, _ := rs.ReadRune()
		buff = append(buff, ch)
	}
	sort.Slice(buff,
		func(i, j int) bool { return buff[i] < buff[j] })
	sb := strings.Builder{}
	for _, e := range buff {
		sb.WriteRune(e)
	}
	return sb.String()
}
