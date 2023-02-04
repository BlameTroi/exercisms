// Package series implements a subset of Project Euler problem
// 8.
package series

// All returns all the possible n character
// substrings in s in the order that they
// appear.
func All(n int, s string) []string {
	list := []string{}
	if n < 1 || n > len(s) {
		return list
	}
	for i := 0; i+n <= len(s); i++ {
		list = append(list, s[i:i+n])
	}
	return list
}

// UnsafeFirst just can't work according to
// the specification. It demonstrates the need
// for multiple return values, so see First.
func UnsafeFirst(n int, s string) string {
	if len(s) != 0 && n <= len(s) {
		return All(n, s)[0]
	}
	if n < 1 {
		return ""
	}
	return s
}

// First returns the first substring of length n from
// s. A value of false for 'ok' indicates that no
// valid substring exists.
func First(n int, s string) (first string, ok bool) {
	list := All(n, s)
	if len(list) == 0 {
		return "", false
	}
	return All(n, s)[0], true
}
