// Package raindrops converts a number to a string
// of words that might sound like rain based on
// its factors.
package raindrops

import "strconv"

// Convert an integer n to a string. If 3, 5, or 7
// are factors, the corresponding words Pling, Plang,
// and Plong are used to build the result. Otherwise,
// the number is converted to a string of digits.
func Convert(n int) string {
	s := ""
	if n%3 == 0 {
		s = s + "Pling"
	}
	if n%5 == 0 {
		s = s + "Plang"
	}
	if n%7 == 0 {
		s = s + "Plong"
	}
	if s == "" {
		s = strconv.Itoa(n)
	}
	return s
}
