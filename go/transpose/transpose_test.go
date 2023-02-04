package transpose

import (
	"reflect"
	"testing"
)

func TestTranspose(t *testing.T) {
	for _, test := range testCases {
		actual := Transpose(test.input)
		if !reflect.DeepEqual(actual, test.expected) {
			// check for zero length slices
			if len(actual) == 0 || len(test.expected) == 0 {
				t.Fatalf("\n\tTranspose(%q): %s\n\n\tExpected: %q\n\tGot: %q",
					test.input, test.description, test.expected, actual)
			}
			// let's make the error more specific and find the row it's on
			min := min(len(test.expected), len(actual))
			for i := 0; i < min; i++ {
				if test.expected[i] != actual[i] {
					t.Fatalf("\n\tTranspose(%q): %s\n\n\tExpected: %q\n\tGot: %q\n\n\tRow %d Expected: %q Got: %q",
						test.input, test.description, test.expected, actual, i, test.expected[i], actual[i])
				}
			}
		}
	}
}

// helper function
// https://stackoverflow.com/questions/27516387/what-is-the-correct-way-to-find-the-min-between-two-integers-in-go
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

var fmtstr = `
Transpose(%q)
      got(%q)
	 want(%q) 
`

func Test_Empty(t *testing.T) {
	input := []string{}
	want := []string{}
	got := Transpose(input)
	if !reflect.DeepEqual(want, got) {
		t.Errorf(fmtstr, input, got, want)
	}
}

func Test_1x1(t *testing.T) {
	input := []string{"A"}
	want := []string{"A"}
	got := Transpose(input)
	if !reflect.DeepEqual(want, got) {
		t.Errorf(fmtstr, input, got, want)
	}
}

func Test_2x1(t *testing.T) {
	input := []string{"A1"}
	want := []string{"A", "1"}
	got := Transpose(input)
	if !reflect.DeepEqual(want, got) {
		t.Errorf(fmtstr, input, got, want)
	}
}

func Test_2x2(t *testing.T) {
	input := []string{"AB", "CD"}
	want := []string{"AC", "BD"}
	got := Transpose(input)
	if !reflect.DeepEqual(want, got) {
		t.Errorf(fmtstr, input, got, want)
	}
}

func Test_PadLeft(t *testing.T) {
	input := []string{"AB", "CDE"}
	want := []string{"AC", "BD", " E"}
	got := Transpose(input)
	if !reflect.DeepEqual(want, got) {
		t.Errorf(fmtstr, input, got, want)
	}
}
func Test_NoPadRight(t *testing.T) {
	input := []string{"ABC", "DE"}
	want := []string{"AD", "BE", "C"}
	got := Transpose(input)
	if !reflect.DeepEqual(want, got) {
		t.Errorf(fmtstr, input, got, want)
	}
}

func Test_PadLeftNoPadRight(t *testing.T) {
	input := []string{"AB", "CDE", "FG", "HJ"}
	want := []string{"ACFH", "BDGJ", " E"}
	got := Transpose(input)
	if !reflect.DeepEqual(want, got) {
		t.Errorf(fmtstr, input, got, want)
	}
}

func Test_2xV(t *testing.T) {
	input := []string{"The fourth line.", "The fifth line."}
	want := []string{
		"TT",
		"hh",
		"ee",
		"  ",
		"ff",
		"oi",
		"uf",
		"rt",
		"th",
		"h ",
		" l",
		"li",
		"in",
		"ne",
		"e.",
		"."}
	got := Transpose(input)
	if !reflect.DeepEqual(want, got) {
		t.Errorf(fmtstr, input, got, want)
	}
}

func BenchmarkTranspose(b *testing.B) {
	if testing.Short() {
		b.Skip("skipping benchmark in short mode.")
	}
	for i := 0; i < b.N; i++ {
		for _, test := range testCases {
			Transpose(test.input)
		}
	}
}
