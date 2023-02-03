// Package reverse reverses a string.
package reverse

// Reverse returns a string with its runes in reverse order.
func Reverse(s string) string {
	if len(s) < 2 {
		return s
	}
	r := ""
	for _, c := range s {
		r = string(c) + r
	}
	return r
}
