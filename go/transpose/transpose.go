package transpose

const (
	maxUint = ^uint(0)
	minUint = 0
	maxInt  = int(maxUint >> 1)
	minInt  = -maxInt - 1
)

func Transpose(array []string) []string {

	// if it's empty, return empty
	if len(array) == 0 {
		return []string{}
	}

	// rows in result = maxcols
	maxcols := minInt
	for _, row := range array {
		if len(row) > maxcols {
			maxcols = len(row)
		}
	}

	// preallocate
	transposed := make([]string, maxcols)

	// flip columns to rows
	for c := 0; c < maxcols; c++ {

		// prealocate
		t2 := make([]rune, len(array))

		for r := 0; r < len(array); r++ {
			// if this row is short, plug a 0x00
			// as a marker otherwise use the
			// value from the row
			if len(array[r]) <= c {
				t2[r] = 0
			} else {
				t2[r] = []rune(array[r])[c]
			}
		}

		// remove any trailing markers
		for t2[len(t2)-1] == 0 {
			t2 = t2[:len(t2)-1]
		}

		// replace any leading markers with
		// spaces
		for i := 0; i < len(t2); i++ {
			if t2[i] == 0 {
				t2[i] = ' '
			}
		}

		// and store the row
		transposed[c] = string(t2)
	}

	return transposed
}
