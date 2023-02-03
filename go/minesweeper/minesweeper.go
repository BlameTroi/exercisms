package minesweeper

import "errors"

func (b Board) Count() error {

	// dimensions must be of a regular rectangle at least two
	// across and two down.
	rows := len(b)
	if rows < 2 {
		return errors.New("bad height")
	}

	cols := len(b[0])
	if cols < 2 {
		return errors.New("bad witdh")
	}

	for _, row := range b {
		if len(row) != cols {
			return errors.New("jagged")
		}
	}

	// borders must have corners of '+', vertical of '|',
	// and horizontal of '-'
	if b[0][0] != '+' || b[0][cols-1] != '+' ||
		b[rows-1][0] != '+' || b[rows-1][cols-1] != '+' {
		return errors.New("invalid border: corner")
	}

	for c := 1; c < cols-1; c++ {
		if b[0][c] != '-' || b[rows-1][c] != '-' {
			return errors.New("invalid border: horizontal")
		}
	}

	for r := 1; r < rows-1; r++ {
		if b[r][0] != '|' || b[r][cols-1] != '|' {
			return errors.New("invalid border: vertical")
		}
	}

	// interior must be blanks or mines '*'
	for r := 1; r < rows-1; r++ {
		for c := 1; c < cols-1; c++ {
			if b[r][c] != ' ' && b[r][c] != '*' {
				return errors.New("invalid character")
			}
		}
	}

	// scan and fill
	for r := 1; r < rows-1; r++ {
		for c := 1; c < cols-1; c++ {
			n := countAround(b, r, c)
			if n == 0 {
				continue
			}
			b[r][c] = byte('0' + n)
		}
	}

	return nil
}

// count mines around a grid. edges are never checked
// so there's no need to watch for boundary conditions.
func countAround(b Board, r int, c int) int {
	n := 0

	// no count needed if this is a mine
	if b[r][c] == '*' {
		return 0
	}

	// scan surrounding grids
	for y := r - 1; y < r+2; y++ {
		for x := c - 1; x < c+2; x++ {
			if b[y][x] == '*' {
				n = n + 1
			}
		}
	}

	return n
}
