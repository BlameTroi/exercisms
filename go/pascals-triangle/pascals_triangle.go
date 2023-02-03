// Package pascal calculates said triangle.
package pascal

// Compute Pascal's triangle up to a given number of rows.
//
// In Pascal's Triangle each number is computed by adding the
// numbers to the right and left of the current position in
// the previous row.
//
//     1
//    1 1
//   1 2 1
//  1 3 3 1
// 1 4 6 4 1
// # ... etc
//

// func Triangle returns a Pascal's triangle of a number of
// rows.
func Triangle(rows int) [][]int {
	t := make([][]int, rows)
	t[0] = []int{1}
	for i := 1; i < rows; i++ {
		t[i] = make([]int, i+1)
		t[i][0] = 1
		for j := 1; j <= i; j++ {
			var l, r int
			l = t[i-1][j-1]
			if j < len(t[i-1]) {
				r = t[i-1][j]
			}
			t[i][j] = l + r
		}
	}

	return t
}
