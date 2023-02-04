// Package diamond prints a 'diamond' using
// uppercase letters.
package diamond

import (
	"errors"
	"strings"
)

// Gen returns a multiline string holding with
// a representation of a diamon drawn using
// uppercase letters.
func Gen(b byte) (string, error) {
	if b < 'A' || b > 'Z' {
		return "", errors.New("invalid")
	}
	// create top half half in bytes, bottom
	// half will be mirroed
	lines := make([][]byte, 0)
	for c := byte('A'); c <= b; c++ {
		line := make([]byte, 1+2*(b-'A'))
		for i := 0; i < len(line); i++ {
			line[i] = ' '
		}
		if c == 'A' {
			line[b-byte('A')] = 'A'
		} else {
			line[b-c] = c
			line[len(line)-1-int(b-c)] = c
		}
		lines = append(lines, line)
	}

	// top half to multiline string
	sb := strings.Builder{}
	for i := 0; i < len(lines); i++ {
		sb.WriteString(string(lines[i]))
		sb.WriteByte('\n')
	}
	// bottom half is a mirror so we work
	// from front to back, but the 'last'
	// line must be skipped since it should
	// only appear once.
	for i := len(lines) - 2; i >= 0; i-- {
		sb.WriteString(string(lines[i]))
		sb.WriteByte('\n')
	}
	return sb.String(), nil
}
