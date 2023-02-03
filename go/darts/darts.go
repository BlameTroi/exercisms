// Package darts provides a simple scoring for a dart toss.
package darts

import "math"

// Function Score takes the coordinates of a dart and
// calculates the score based on distance from the target
// center. Landing exactly on a ring boundary favors the
// higher or inner score.
//
// The radii of the rings are:
// inner	1
// middle	5
// outer	10
//
// Distances <= 1.0 earn 10 points
//           <= 5.0 earn 5 points
//           <= 10.0 earn 1 point
//           >  10.0 earn 0 points
func Score(x, y float64) int {
	// distance from origin
	d := math.Sqrt(x*x + y*y)
	// score
	if d > 10.0 {
		return 0
	}
	if d > 5.0 {
		return 1
	}
	if d > 1.0 {
		return 5
	}
	return 10
}
