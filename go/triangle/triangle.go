// Package triangle determines if a triangle is equilateral,
// isosceles, or scalene.
package triangle

import "math"

// The Kind of the triangle.
type Kind uint

// The various kinds of triangle. These values fit in Kind.
const (
	NaT = iota // not a triangle
	Equ = iota // equilateral
	Iso = iota // isosceles
	Sca = iota // scalene
)

// KindFromSides determines if a triangle of sides a, b, and c
// could exist (is this a valid triangle), and if so is the
// triangle an equalteral, isosceles, or scalene.
func KindFromSides(a, b, c float64) Kind {
	// Are there any zero or negative sides?
	if a <= 0 || b <= 0 || c <= 0 {
		return NaT
	}
	// Are there any extreme values (infinities, non-numbers)?
	if math.IsInf(a, 0) || math.IsInf(b, 0) || math.IsInf(c, 0) ||
		math.IsNaN(a) || math.IsNaN(b) || math.IsNaN(c) {
		return NaT
	}
	// Is this an equilateral? I.E., all three sides equal?
	if a == b && b == c {
		return Equ
	}
	// To be a valid triangle the sum of the lengths of any two sides
	// must be greater than or equal to the length of the third side.
	if (a+b) < c || (a+c) < b || (b+c) < a {
		return NaT
	}
	// Is this an isosceles?
	if a == b || a == c || b == c {
		return Iso
	}
	// if we fall through all the prior tests, we have a valid
	// triangle with three unequal sides: a scalene.
	return Sca
}
