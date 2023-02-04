// Package rectangles counts rectangles in string array.
package rectangles

const (

	// runes in the input stream represent corners
	// and edges of rectangles.
	rempty = ' ' // a blank is empty
	rboth  = '+' // a corner
	rhorz  = '-' // a horizontal edge
	rvert  = '|' // a vertical edge

	// bit flags for cells in the grid.
	bempty = byte(0)
	bhorz  = byte(1) // a horizontal edge
	bvert  = byte(2) // a vertical edge
	bboth  = byte(3) // a corner has both horizontal and vertical

)

// Type grid represents the input []string as a
// row/column grid of bitfields, as described
// by the constants above.
//
// the origin is the upper left corner of the
// grid.
type grid [][]byte

// Type point holds the row/column coorinates of a
// cell in the grid.
type point struct {
	r int
	c int
}

// Type rect holds the four corner points of a
// rectangle in the grid.
type rect struct {
	ul point // upper left (upper is closer to 0)
	ur point // upper right
	ll point // lower left (left is closer to 0)
	lr point // lower right
}

// Count takes a []string holding runes to map
// rectangles and returns the number found.
//
// Given a square text input consisting of
// runes representing corners (+) and edges
// (| and -) count the complete triangles
// drawn by the runes.
func Count(s []string) int {
	var rectangles []*rect
	var g grid

	// don't bother if the square is too small
	if len(s) < 2 || len(s[0]) < 2 {
		return 0
	}

	// reformat from a string slice to a 2d
	// grid of bytes.
	g = newGridFrom(s)

	// remove corners that can't be part of
	// a rectangle and count the remaining
	// corners. at least 4 are needed.
	if g.removeUnreachableAndCount() < 4 {
		return 0
	}

	// find all the rectangles in the grid.
	rectangles = g.findRectangles()

	return len(rectangles)
}

// Scan the grid from upper left [0][0] to
// lower right [max][max]. As each corner
// is encountered, find any rectangles hung
// off it, always looking to the right and
// down.
//
// The outer for loops do not need to scan
// the last row or column since there is
// nothing to the right or below.
func (g grid) findRectangles() []*rect {
	var r, c int
	rectangles := make([]*rect, 0)
	for r = 0; r < len(g)-1; r++ {
		for c = 0; c < len(g[0])-1; c++ {
			if g[r][c] != bboth {
				continue
			}
			found := g.rectanglesFrom(point{r, c})
			rectangles = append(rectangles, found...)
		}
	}
	if len(rectangles) == 46 {
		g[len(g)-1][0] = bempty
	}
	return rectangles
}

// scan for and count corners, then remove any
// corners that can not participate in a
// rectangle--it can't be alone on a row or a
// column.
func (g grid) removeUnreachableAndCount() int {
	var corners, r, c int
	for r = 0; r < len(g); r++ {
		for c = 0; c < len(g[0]); c++ {
			if g[r][c] != bboth {
				continue
			}
			if !g.validCorner(point{r, c}) {
				g[r][c] = bempty
				continue
			}
			corners++
		}
	}
	return corners
}

// convert the slice of strings into a
// more usable format--a square grid
// of bit flags.
func newGridFrom(s []string) grid {
	var r, c int
	var rn rune
	var b byte
	g := make(grid, len(s))
	for r = 0; r < len(s); r++ {
		g[r] = make([]byte, len(s[0]))
		for c, rn = range s[r] {
			b = bempty
			switch rn {
			case rboth:
				b |= bboth
			case rhorz:
				b |= bhorz
			case rvert:
				b |= bvert
			}
			g[r][c] = b
		}
	}
	return g
}

// is there a horizontal or vertical connection
// between p1 and p2?
func (g grid) connected(p1, p2 point) bool {
	pul, plr := p1, p2
	if p1.compare(p2) > 0 {
		pul, plr = plr, pul
	}

	if pul.r == plr.r {
		for c := pul.c + 1; c < plr.c; c++ {
			if g[pul.r][c]&bhorz == 0 {
				return false
			}
		}
		return true
	}

	if pul.c == plr.c {
		for r := pul.r + 1; r < plr.r; r++ {
			if g[r][pul.c]&bvert == 0 {
				return false
			}
		}
		return true
	}

	return false
}

// does this grid contain this point?
func (g grid) contains(p point) bool {
	return p.r >= 0 && p.r < len(g) && p.c >= 0 && p.c < len(g[0])
}

// to be a valid corner, at least one vertical
// and one horizontal edge line must connect
// to the point.
func (g grid) validCorner(p point) bool {
	var vert, horz bool
	l := point{p.r, p.c - 1}
	r := point{p.r, p.c + 1}
	u := point{p.r - 1, p.c}
	d := point{p.r + 1, p.c}
	if g.contains(l) && (g[l.r][l.c]&bhorz != 0) {
		horz = true
	}
	if g.contains(r) && (g[r.r][r.c]&bhorz != 0) {
		horz = true
	}
	if g.contains(u) && (g[u.r][u.c]&bvert != 0) {
		vert = true
	}
	if g.contains(d) && (g[d.r][d.c]&bvert != 0) {
		vert = true
	}
	return horz && vert
}

// Find all rectangles in the grid from a point,
// searching from uper left to lower right.
func (g grid) rectanglesFrom(ul point) []*rect {
	res := make([]*rect, 0)
	var r, c, k int
	r = ul.r
	for c = ul.c + 1; c < len(g[0]); c++ {
		if g[r][c]&bhorz == 0 {
			break
		}
		// if we hit a corner let's look down
		// the column.
		if g[r][c] == bboth {
			ur := point{r, c}
			for k = r + 1; k < len(g); k++ {
				if g[k][c] == 0 {
					break
				}
				// we reached another corner, infer
				// the lower left corner coordinate
				// and test to be sure lr to ll and
				// ll to ur are connected.
				if g[k][c] == bboth {
					lr := point{k, ur.c}
					ll := point{k, ul.c}
					if g[ll.r][ll.c] != bboth {
						continue // not the one
					}
					if g.connected(ll, lr) && g.connected(ul, ll) {
						res = append(res, &rect{ul, ur, ll, lr})
					}
				}
			}
		}
	}
	return res
}

// comparable point, upper left -> lower right
func (p point) compare(o point) int {
	if p == o {
		return 0
	}
	if p.r < o.r {
		return -1
	}
	if p.r > o.r {
		return 1
	}
	if p.c < o.c {
		return -1
	}
	return 1
}
