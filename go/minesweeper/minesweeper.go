package minesweeper

func Annotate(s []string) []string {

	// guard clauses: garbage in = same garbage out
	if len(s) == 0 || len(s[0]) == 0 {
		return s
	}

	rows := len(s)
	cols := len(s[0])

	if rows < 2 || cols < 2 {
		return s
	}

	for _, row := range s {
		if len(row) != cols {
			return s
		}
	}

	// this works better as runes ...
	b := make([][]rune, rows)
	for i, row := range s {
		b[i] = []rune(row)
	}

	// update board in place
	for r := 0; r < rows; r = r + 1 {
		for c := 0; c < cols; c = c + 1 {
			if b[r][c] == '*' {
				// mined grids don't need counts
				continue
			}

			// scan surrounding grids
			n := 0
			for y := r - 1; y < r+2; y++ {
				if y < 0 || y >= rows {
					continue
				}
				for x := c - 1; x < c+2; x++ {
					if x < 0 || x >= cols {
						continue
					}
					if b[y][x] == '*' {
						n = n + 1
					}
				}
			}
			if n > 0 {
				b[r][c] = rune(n + '0')
			}
		}
	}

	// reformat back to []string format
	r := make([]string, rows)
	for i, runeRow := range b {
		r[i] = string(runeRow)
	}

	return r
}

// count mines around a grid. edges are never checked
// so there's no need to watch for boundary conditions.
func countAround(b [][]rune, r int, c int) int {
	n := 0

	// no count needed if this is a mine
	if b[r][c] == '*' {
		return 0
	}

	return n
}
