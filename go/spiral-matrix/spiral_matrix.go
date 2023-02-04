package spiralmatrix

type movement struct {
	r int
	c int
}

var movements = map[rune]movement{
	'l': {0, -1},
	'r': {0, 1},
	'u': {-1, 0},
	'd': {1, 0},
}
var nextDirection = map[rune]rune{
	'l': 'u',
	'r': 'd',
	'u': 'r',
	'd': 'l',
}

func SpiralMatrix(s int) [][]int {
	if s == 0 {
		return [][]int{}
	}
	if s == 1 {
		return [][]int{{1}}
	}
	matrix := makeMatrix(s)
	r := 0
	c := 0
	direction := 'r'
	moveToNext := movements[direction]
	n := 1
	for n <= s*s {
		if r >= 0 && r < s &&
			c >= 0 && c < s &&
			matrix[r][c] == 0 {
			matrix[r][c] = n
			n += 1
			r += moveToNext.r
			c += moveToNext.c
			continue
		}
		r -= moveToNext.r
		c -= moveToNext.c
		direction = nextDirection[direction]
		moveToNext = movements[direction]
		r += moveToNext.r
		c += moveToNext.c
	}
	return matrix
}

func makeMatrix(s int) [][]int {
	matrix := make([][]int, s)
	for i := 0; i < s; i++ {
		matrix[i] = make([]int, s)
	}
	return matrix
}
