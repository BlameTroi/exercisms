//go:build !bonus
// +build !bonus

package robotname

import (
	"testing"
)

func TestNameValid(t *testing.T) {
	n := New().getName(t, false)
	if !namePat.MatchString(n) {
		t.Errorf(`Invalid robot name %q, want form "AA###".`, n)
	}
}

func TestNameSticks(t *testing.T) {
	r := New()
	n1 := r.getName(t, false)
	n2 := r.getName(t, true)
	if n2 != n1 {
		t.Errorf(`Robot name changed.  Now %s, was %s.`, n2, n1)
	}
}

func TestSuccessiveRobotsHaveDifferentNames(t *testing.T) {
	n1 := New().getName(t, false)
	n2 := New().getName(t, false)
	if n1 == n2 {
		t.Errorf(`Robots with same name.  Two %s's.`, n1)
	}
}

func TestResetName(t *testing.T) {
	r := New()
	n1 := r.getName(t, false)
	r.Reset()
	if r.getName(t, false) == n1 {
		t.Errorf(`Robot name not cleared on reset.  Still %s.`, n1)
	}
}

// Note if you go for bonus points, this benchmark likely won't be
// meaningful.  Bonus thought exercise, why won't it be meaningful?
func BenchmarkName(b *testing.B) {
	// Benchmark combined time to create robot and name.
	for i := 0; i < b.N; i++ {
		New().getName(b, false)
	}
}

func Test_generate(t *testing.T) {
	tests := []struct {
		name  string
		count int
	}{
		{"5k tries", 50000},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			for i := 0; i < tt.count; i++ {
				got := candidate()
				if !namePat.MatchString(got) {
					t.Errorf("candidate() = %v", got)
				}
			}
		})
	}
}

// generate 10000 random letters, check the distribution
func Test_distribution_letters(t *testing.T) {
	letters := map[rune]int{}
	for i := 0; i < 100000; i++ {
		letters[letter()]++
	}
	minr, minc, maxr, maxc := '*', 100000, '*', -100000
	for r, c := range letters {
		if c < minc {
			minr, minc = r, c
		}
		if c > maxc {
			maxr, maxc = r, c
		}
		//t.Errorf("%c %d", r, c)
	}
	if len(letters) != 26 {
		t.Errorf("%d\t%c %d\t%c %d", len(letters), minr, minc, maxr, maxc)
	}
}
