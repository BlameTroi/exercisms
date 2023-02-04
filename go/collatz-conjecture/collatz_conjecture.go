// Package colatzconjecture presents an implementation
// Colatz's 3n+1 problem.
package collatzconjecture

import "errors"

// Function CollatzConjecture returns the number of
// iterations through Collatz's 3n+1 algorithm for
// to reach a value of 1. An error is returned if
// the start value is less than 1.
//
// Input is converted to an int64 to reduce the chance
// of an overflow while processing.
func CollatzConjecture(start int) (int, error) {
	if start < 1 {
		return 0, errors.New("illegal argument, must be >= 1")
	}
	var n int64
	n = int64(start)
	steps := 0
	for n != 1 {
		steps++
		if n%2 == 0 {
			n = n / 2
		} else {
			n = 1 + n*3
		}
	}
	return steps, nil
}
