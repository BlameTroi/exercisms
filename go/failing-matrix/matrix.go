// Package matrix provides simple access to an array of integers.
package matrix

import (
	"errors"
	"strconv"
	"strings"
)

// Matrix represents a square array of integers.
type Matrix struct {
	rows   int
	cols   int
	values []int
}

// New parses a string of rows (integers delimited by spaces,
// delimited by newlines) into a Matrix.
func New(s string) (*Matrix, error) {
	m := Matrix{}
	m.values = []int{}
	rowcount, colcount := 0, 0
	for _, r := range strings.Split(s, "\n") {
		rowcount++
		cc := 0
		for _, n := range strings.Split(strings.TrimSpace(r), " ") {
			cc++
			a, err := strconv.Atoi(n)
			if err != nil {
				return nil, errors.New("invalid numeric value found")
			}
			m.values = append(m.values, a)
		}
		if cc == 0 {
			return nil, errors.New("empty row found")
		}
		if colcount == 0 {
			colcount = cc
		}
		if cc != colcount {
			return nil, errors.New("not a square matrix, ragged columns")
		}
	}
	m.rows, m.cols = rowcount, colcount
	return &m, nil
}

// Cols returns a copy of the Matrix as a slice of columns.
func (m *Matrix) Cols() [][]int {
	var res [][]int
	for c := 0; c < m.cols; c++ {
		var col []int
		for r := 0; r < m.rows; r++ {
			col = append(col, m.values[r*m.cols+c])
		}
		res = append(res, col)
	}
	return res
}

// Rows returns a copy of the Matrix as a slice of rows.
func (m *Matrix) Rows() [][]int {
	var res [][]int
	for r := 0; r < m.rows; r++ {
		row := make([]int, m.cols)
		copy(row, m.values[r*m.cols:r*m.cols+m.cols])
		res = append(res, row)
	}
	return res
}

// Set updates one value in the Matrix. Invalid
// row,col will return false.
func (m *Matrix) Set(row, col, val int) bool {
	if row < 0 || row >= m.rows ||
		col < 0 || col >= m.cols {
		return false
	}
	m.values[row*m.cols+col] = val
	return true
}
