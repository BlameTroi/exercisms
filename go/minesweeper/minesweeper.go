// Package minesweeper updates the map in a game of minesweeper
// to reflect the number of mines in the grids adjacent to a
// empty grid. A mine is marked by a '*' and adjacent mines are
// ' ' for zero, or '1..8'.

package minesweeper

// min returns the smaller of two integers.
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// max returns the larger of two integers.
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// Annotate takes a minesweeper map as an array of strings where
// grid positions are either occupied by a mine ('*') or empty
// (' ') and returns an updated map as an array of strings where
// empty positions now report the number of adjacent mines (blank
// for zero, or 1-8).
func Annotate(s []string) []string {

	// guard clauses: garbage in = same garbage out
	if len(s) == 0 || len(s[0]) == 0 {
		return s
	}

	rows := len(s)
	cols := len(s[0])

	// too small ot worry about
	if rows < 1 || cols < 1 {
		return s
	}

	// not rectangular
	for _, row := range s {
		if len(row) != cols {
			return s
		}
	}

	// a rune grid is a better way to view the data
	b := make([][]rune, rows)
	for i, row := range s {
		b[i] = []rune(row)
	}

	// scan for mines and update counts in rune grid
	for r := 0; r < rows; r = r + 1 {
		for c := 0; c < cols; c = c + 1 {

			// mined grids don't need counts
			if b[r][c] == '*' {
				continue
			}

			// scan surrounding grids
			n := 0

			for sr := max(r-1, 0); sr <= min(rows-1, r+1); sr = sr + 1 {
				for sc := max(c-1, 0); sc <= min(cols-1, c+1); sc = sc + 1 {
					if b[sr][sc] == '*' {
						n = n + 1
					}
				}
			}

			// store count if non zero
			if n > 0 {
				b[r][c] = rune(n + '0')
			}
		}
	}

	// convert runes back into []string
	r := make([]string, rows)
	for i, runeRow := range b {
		r[i] = string(runeRow)
	}

	return r
}
