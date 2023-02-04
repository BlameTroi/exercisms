// Package matrix provides simple access to an array of integers.
package matrix

import (
	"errors"
	"strconv"
	"strings"
)

// Matrix represents a square array of integers.
//
// REMEMBER: slices are not arrays.
type Matrix [][]int

// New parses a string of rows (integers delimited by spaces,
// delimited by newlines) into a Matrix.
func New(s string) (Matrix, error) {
	m := Matrix{}
	rowcount, colcount := 0, 0
	for _, r := range strings.Split(s, "\n") {
		rowcount++
		row := []int{}
		cc := 0
		for _, n := range strings.Split(strings.TrimSpace(r), " ") {
			cc++
			a, err := strconv.Atoi(n)
			if err != nil {
				return nil, errors.New("invalid numeric value found")
			}
			row = append(row, a)
		}
		m = append(m, row)
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
	return m, nil
}

// Cols returns a copy of the Matrix as a slice of columns.
func (m Matrix) Cols() [][]int {
	var res [][]int
	for i := 0; i < len(m[0]); i++ {
		col := []int{}
		for j := 0; j < len(m); j++ {
			col = append(col, m[j][i])
		}
		res = append(res, col)
	}
	return res
}

// Rows returns a copy of the Matrix as a slice of rows.
func (m Matrix) Rows() [][]int {
	var res [][]int
	for i := 0; i < len(m); i++ {
		row := make([]int, len(m[i]))
		copy(row, m[i])
		res = append(res, row)
	}
	return res
}

// Set updates one value in the Matrix. Invalid
// row,col will return false.
func (m Matrix) Set(row, col, val int) bool {
	if row < 0 || row >= len(m) || col < 0 || col >= len(m[0]) {
		return false
	}
	m[row][col] = val
	return true
}
