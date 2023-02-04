// Package etl provides a transform for Scrabble scoring data.
package etl

import "strings"

// Transform Scrabble scoring data from keyed on score to
// being keyed on letter. Letters are also lower cased.
func Transform(in map[int][]string) map[string]int {
	out := map[string]int{}
	for score, letters := range in {
		for _, letter := range letters {
			out[strings.ToLower(letter)] = score
		}
	}
	return out
}
