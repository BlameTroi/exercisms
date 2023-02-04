// Package diffsquares is for Project Euler Problem 6.
package diffsquares

// SquareOfSum squares the sum of integers in a given range.
// Gauss showed us how to do this quickly.
func SquareOfSum(to int) int {
	s := ((to * (to + 1)) / 2)
	return s * s
}

// SumOfSquares sums the squares of integers in a given range.
//
// Iteration one is done brute force. Derive a formula to avoid
// the looping for iteration two.
//
// Iteration two relies on the formula:
//
// (n(n+1)(2n+1))/6
//
// Found after some web searching.
func SumOfSquares(to int) int {
	return (to * (to + 1) * (to + to + 1)) / 6
}

// Difference calculates the difference between the square of
// the sum of an integer range and the sum of the ranges' squares.
//
// Iteration one relies completely on the prior two functions.
// I don't think the overhead of function calls is significant
// enough to justify not reusing the functions.
func Difference(to int) int {
	return SquareOfSum(to) - SumOfSquares(to)
}
