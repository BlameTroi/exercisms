// Package grains holds a solution for an exercism drill.
package grains

import "errors"

// Square returns the number of grains of wheat on a specifc
// square in the wheat and chessboard problem. The problem
// illustrates powers of two as they apply to integers in a
// computer, yielding a fast result.
func Square(n int) (uint64, error) {
	if n < 1 || n > 64 {
		return 0, errors.New("value out of range 1-64 inclusive")
	}
	var r uint64 = 1 << (n - 1)
	return r, nil
}

// Total returns the total number of grains of wheat on the
// whole board in the wheat and chessboard problem. Again,
// thinking in terms of powers of two yields a fast result.
func Total() uint64 {

	var r uint64 = 1<<64 - 1
	return r
}
