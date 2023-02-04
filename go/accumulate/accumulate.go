// Package accumulate provides a mapping function for
// collections of strings.
package accumulate

// Accumulate is an implementation of the classic mapping
// function on collections. Given an array of strings and
// a function, create a new array holding the result of
// running the function on each entry of the original
// array.
func Accumulate(c []string, f func(string) string) []string {
	var r []string
	for _, i := range c {
		r = append(r, f(i))
	}
	return r
}
